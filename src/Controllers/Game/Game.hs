module Controllers.Game.Game
  ( performRequest,
    withNotifyJoinAndLeave,
    updateUserChannels,
  )
where

import ClassyPrelude (getCurrentTime, putStrLn, traverse_, whenM)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO)
import Control.Exception (bracket_)
import System.Timeout (timeout)
import Control.Monad
import Control.Monad.STM
import Controllers.Chat.Chatroom
import qualified Controllers.Chat.Chatroom as CR (ChatMessage (ChatMessage), Chatroom, SendMessage (SendMessage), sendMessage)
import Controllers.Definition.DefinitionService (Definition (Definition), DefinitionServiceImpl, withDefinitionsAsync)
import Controllers.Game.Api
import Controllers.Game.GameDefinitionController (DefinitionResponse (..), GameDefinitionController, storeGameDefinitions)
import Controllers.Game.Model.ServerGame
import qualified Controllers.Game.Model.ServerPlayer as SP
import qualified Controllers.Game.Persist as P
import Controllers.User.Model.AuthUser
import Data.Conduit
import qualified Data.List as L
import qualified Data.Map as M
import Data.Pool
import Data.Text
import Data.Time
import Database.Persist.Sql
import GHC.IO
import Model (User)
import Repository.DefinitionRepository (DefinitionRepositoryImpl, WordDefinitionItem (WordDefinitionItem), saveGameDefinitionsImpl)
import Wordify.Rules.FormedWord
import Wordify.Rules.Game
import Wordify.Rules.LetterBag
import Wordify.Rules.Move
import Wordify.Rules.Player
import Wordify.Rules.Pos
import Wordify.Rules.ScrabbleError
import Wordify.Rules.Tile
import Prelude
import Model.GameSetup (extraRules, LocalisedGameSetup (gameLanguageShortCode))
import qualified Control.Arrow as A
import Wordify.Rules.Extra.ExtraRule (applyExtraRules, finalTransition, RuleApplicationsResult (finalTransition), RuleApplicationResult (transition))
import Controllers.Common.CacheableSharedResource (ResourceCache, peekCacheableResource)
import Controllers.Game.Model.UserEventSubscription
import Controllers.Push.PushController

handlePlayerConnect :: Pool SqlBackend -> ServerGame -> Maybe AuthUser -> IO ()
handlePlayerConnect pool serverGame Nothing = pure ()
handlePlayerConnect pool serverGame (Just user) = do
  now <- getCurrentTime
  persistPlayerLastSeen pool user (gameId serverGame) now
  atomically $ handleNewPlayerConnection serverGame user now
  where
    handleNewPlayerConnection :: ServerGame -> AuthUser -> UTCTime -> STM ()
    handleNewPlayerConnection serverGame user now = do
      newConnectionCount <- increasePlayerConnections serverGame user now
      when (isFirstConnection newConnectionCount) $ do
        notifyPlayerConnect serverGame user now

    isFirstConnection :: Maybe Int -> Bool
    isFirstConnection (Just connectionCount) = connectionCount == 1
    isFirstConnection Nothing = False

handlePlayerDisconnect :: Pool SqlBackend -> ServerGame -> Maybe AuthUser -> IO ()
handlePlayerDisconnect pool serverGame Nothing = pure ()
handlePlayerDisconnect pool serverGame (Just user) = do
  now <- getCurrentTime
  persistPlayerLastSeen pool user (gameId serverGame) now
  atomically $ handleNewPlayerDisconnection serverGame user now
  where
    handleNewPlayerDisconnection :: ServerGame -> AuthUser -> UTCTime -> STM ()
    handleNewPlayerDisconnection serverGame user now = do
      newConnectionCount <- decreasePlayerConnections serverGame user now
      when (isLastConnection newConnectionCount) $ do
        notifyPlayerDisconnect serverGame user now

    isLastConnection :: Maybe Int -> Bool
    isLastConnection (Just connectionCount) = connectionCount == 0
    isLastConnection Nothing = False

notifyPlayerConnect :: ServerGame -> AuthUser -> UTCTime -> STM ()
notifyPlayerConnect serverGame user@(AuthUser userid _) now = do
  let playerNumber = getPlayerNumber serverGame user
  traverse_ (writeTChan (broadcastChannel serverGame) . flip PlayerConnect now) playerNumber

notifyPlayerDisconnect :: ServerGame -> AuthUser -> UTCTime -> STM ()
notifyPlayerDisconnect serverGame user now = do
  let playerNumber = getPlayerNumber serverGame user
  traverse_ (writeTChan (broadcastChannel serverGame) . flip PlayerDisconnect now) playerNumber

persistPlayerLastSeen :: Pool SqlBackend -> AuthUser -> Text -> UTCTime -> IO ()
persistPlayerLastSeen pool (AuthUser userId _) gameId = P.updatePlayerLastSeen pool gameId userId

withNotifyJoinAndLeave :: Pool SqlBackend -> ServerGame -> Maybe AuthUser -> IO () -> IO ()
withNotifyJoinAndLeave pool serverGame maybeUser = bracket_ (handlePlayerConnect pool serverGame maybeUser) (handlePlayerDisconnect pool serverGame maybeUser)

performRequest :: ServerGame -> Chatroom -> GameDefinitionController -> Pool SqlBackend ->  ResourceCache Text (TChan UserEvent) -> PushController -> Maybe AuthUser -> ClientMessage -> IO ServerResponse
performRequest serverGame _ _ pool userChannelSubscriptions pushCtrl player (BoardMove placed) =
  handleBoardMove serverGame pool userChannelSubscriptions pushCtrl (player >>= getPlayerNumber serverGame) placed
performRequest serverGame _ _ pool userChannelSubscriptions pushCtrl player (ExchangeMove exchanged) =
  handleExchangeMove serverGame pool userChannelSubscriptions pushCtrl (player >>= getPlayerNumber serverGame) exchanged
performRequest serverGame _ _ pool userChannelSubscriptions pushCtrl player PassMove =
  handlePassMove serverGame pool userChannelSubscriptions pushCtrl (player >>= getPlayerNumber serverGame)
performRequest serverGame chatroom _ pool _ _ player (SendChatMessage msg) =
  handleChatMessage serverGame chatroom pool player msg
performRequest serverGame _ _ pool userChannelSubscriptions _ player (AskPotentialScore placed) =
  handlePotentialScore serverGame placed
performRequest serverGame _ gameDefinitionWorker _ userChannelSubscriptions _ player (AskDefinition word) =
  handleAskDefinition gameDefinitionWorker serverGame (player >>= getPlayerNumber serverGame) word

handleAskDefinition :: GameDefinitionController -> ServerGame -> Maybe Int -> Text -> IO ServerResponse
handleAskDefinition definitionWorker serverGame Nothing word =
  return $ InvalidCommand "Observers cannot request definitions."
handleAskDefinition definitionWorker serverGame (Just _) word = do
  let languageShortCode = gameLanguageShortCode (gameSetup serverGame)
  storeGameDefinitions definitionWorker (gameId serverGame) word languageShortCode $ \(DefinitionResponse word time definitions defNumber) -> atomically $ do
    let channel = broadcastChannel serverGame
    writeTChan channel (WordDefinitions word time definitions defNumber)
  pure AskDefinitionSuccess
  where
    wordPlayedInGame :: ServerGame -> Bool
    wordPlayedInGame serverGame = undefined

handlePotentialScore :: ServerGame -> [(Pos, Tile)] -> IO ServerResponse
handlePotentialScore serverGame placedTiles =
  do
    gameState <- readTVarIO $ game serverGame
    let gameBoard = board gameState
    let formedWords =
          if moveNumber gameState > 1
            then wordsFormedMidGame gameBoard (M.fromList placedTiles)
            else wordFormedFirstMove gameBoard (M.fromList placedTiles)

    return $
      case formedWords of
        Left _ -> PotentialScore 0
        Right formed -> PotentialScore (overallScore formed)

{-
    If the move is successful, writes an update onto the broadcast channel and
    returns a server response to send to the moving client. Otherwise, returns
    an error to the client and writes nothing to the channel.
-}
handleMove ::
  ServerGame ->
  Pool SqlBackend ->
  ResourceCache Text (TChan UserEvent) ->
  PushController ->
  Int ->
  Move ->
  (Either Text GameTransition -> ServerResponse) ->
  IO ServerResponse
handleMove serverGame pool userEventChannelSubscriptions pushCtrl playerMoving move moveOutcomeHandler =
  do
    gameSnapshot <- atomically $ makeServerGameSnapshot serverGame
    let currentGameState = gameState gameSnapshot
    if playerNumber currentGameState /= playerMoving
      then return $ InvalidCommand "Not your move"
      else do
        let channel = broadcastChannel serverGame
        let moveOutcome = A.left (pack . show) (makeMove currentGameState move)
        let moveWithLocalisedRulesOutcome = moveOutcome >>= flip applyLocalisedRules (gameSetup serverGame)

        case moveWithLocalisedRulesOutcome of
          Left err -> return $ moveOutcomeHandler $ Left err
          Right transition -> do
            let eventMessage = transitionToMessage transition
            let newGameState = newGame transition

            now <- getCurrentTime
            P.persistGameUpdate pool (snapshotGameId gameSnapshot) newGameState eventMessage

            newSnapshot <- atomically $ do
              writeTVar (game serverGame) newGameState
              updateLastMoveMade serverGame now
              writeTChan channel eventMessage
              makeServerGameSnapshot serverGame

            let nextPlayerUserId = currentPlayerToMove newSnapshot
                isGameOver = case transition of
                  GameFinished _ _ -> True
                  _ -> False
            updateUserChannels userEventChannelSubscriptions newSnapshot nextPlayerUserId isGameOver
            sendMovePushNotification pushCtrl newSnapshot transition nextPlayerUserId
            return $ moveOutcomeHandler moveOutcome

sendMovePushNotification :: PushController -> ServerGameSnapshot -> GameTransition -> Maybe Text -> IO ()
sendMovePushNotification _ _ (GameFinished _ _) _ = pure ()
sendMovePushNotification _ _ _ Nothing = pure ()
sendMovePushNotification pushCtrl snapshot _ (Just nextPlayerId) = do
  _ <- forkIO $ do
    _ <- timeout (10 * 1000000) $
      sendMoveNotification pushCtrl nextPlayerId (snapshotGameId snapshot)
    pure ()
  pure ()

updateUserChannels :: ResourceCache Text (TChan UserEvent) -> ServerGameSnapshot -> Maybe Text -> Bool -> IO ()
updateUserChannels userEventChannelSubscriptions snapshot nextPlayerUserId isGameOver = do
  let players = snapshotPlayers snapshot
      gId = snapshotGameId snapshot
  forM_ players $ \player -> do
    let userIdent = SP.playerId player
    atomically $ do
      maybeChannel <- peekCacheableResource userEventChannelSubscriptions userIdent
      case maybeChannel of
        Nothing -> return ()
        Just channel -> do
          let event = if isGameOver
                then GameOver gId snapshot
                else MoveInUserGame gId snapshot (Just userIdent == nextPlayerUserId)
          writeTChan channel event

applyLocalisedRules :: GameTransition -> LocalisedGameSetup -> Either Text GameTransition
applyLocalisedRules gameTransition localisedGameSetup =
  let extraLocalisationGameRules = extraRules localisedGameSetup
  in let ruleApplicationResult = applyExtraRules gameTransition extraLocalisationGameRules
  in let resultWithTransformedError = A.left (pack. show) ruleApplicationResult
  in finalTransition <$> resultWithTransformedError

handleBoardMove :: ServerGame -> Pool SqlBackend ->  ResourceCache Text (TChan UserEvent) -> PushController -> Maybe Int -> [(Pos, Tile)] -> IO ServerResponse
handleBoardMove _ _ _ _ Nothing _ = return $ InvalidCommand "Observers cannot move"
handleBoardMove sharedServerGame pool userChannelSubscriptions pushCtrl (Just playerNo) placed =
  handleMove
    sharedServerGame
    pool
    userChannelSubscriptions
    pushCtrl
    playerNo
    (PlaceTiles $ M.fromList placed)
    moveOutcomeHandler
  where
    moveOutcomeHandler (Left err) = InvalidCommand err
    moveOutcomeHandler (Right (MoveTransition newPlayer _ _)) = BoardMoveSuccess (tilesOnRack newPlayer)
    moveOutcomeHandler (Right _) = BoardMoveSuccess []

handleExchangeMove :: ServerGame -> Pool SqlBackend ->  ResourceCache Text (TChan UserEvent) -> PushController -> Maybe Int -> [Tile] -> IO ServerResponse
handleExchangeMove _ _ _ _ Nothing _ = return $ InvalidCommand "Observers cannot move"
handleExchangeMove sharedServerGame pool userchannelSubscriptions pushCtrl (Just playerNo) exchanged =
  handleMove
    sharedServerGame
    pool
    userchannelSubscriptions
    pushCtrl
    playerNo
    (Exchange exchanged)
    moveOutcomeHandler
  where
    moveOutcomeHandler (Left err) = InvalidCommand err
    moveOutcomeHandler (Right (ExchangeTransition _ _ afterPlayer _)) = ExchangeMoveSuccess (tilesOnRack afterPlayer)
    moveOutcomeHandler _ = InvalidCommand "internal server error, unexpected transition"

handlePassMove :: ServerGame -> Pool SqlBackend ->  ResourceCache Text (TChan UserEvent) -> PushController -> Maybe Int -> IO ServerResponse
handlePassMove _ _ _ _ Nothing = return $ InvalidCommand "Observers cannot move"
handlePassMove sharedServerGame pool userChannelSubscriptions pushCtrl (Just playerNo) =
  handleMove sharedServerGame pool userChannelSubscriptions pushCtrl playerNo Pass moveOutcomeHandler
  where
    moveOutcomeHandler (Left err) = InvalidCommand err
    moveOutcomeHandler (Right _) = PassMoveSuccess

handleChatMessage :: ServerGame -> Chatroom -> Pool SqlBackend -> Maybe AuthUser -> Text -> IO ServerResponse
handleChatMessage _ _ _ Nothing _ = return $ InvalidCommand "Observers cannot chat."
handleChatMessage serverGame chatroom pool (Just user) messageText =
  do
    now <- getCurrentTime
    gameSnapshot <- atomically $ makeServerGameSnapshot serverGame
    let serverPlayer = getServerPlayerSnapshot gameSnapshot user
    let playerName = serverPlayer >>= SP.name

    case playerName of
      Nothing -> return $ InvalidCommand "Internal server error"
      Just playerName -> sendMessage chatroom (CR.SendMessage playerName messageText) >> return ChatSuccess
