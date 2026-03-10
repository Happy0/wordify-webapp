module Controllers.Game.Game
  ( performRequest,
    withNotifyJoinAndLeave,
  )
where

import ClassyPrelude (getCurrentTime, putStrLn, traverse_, whenM, writeTBChan)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Exception (bracket_)
import Control.Monad
import Control.Monad.STM
import Modules.Chats.Api
import qualified Modules.Chats.Api as CR (ChatMessage (ChatMessage), Chatroom, SendMessage (SendMessage), sendMessage)
import Modules.Definition.DefinitionClient (Definition (Definition))
import Controllers.Game.Api
import Controllers.Game.GameDefinitionController (DefinitionResponse (..), GameDefinitionController, storeGameDefinitions)
import Controllers.Game.Model.ServerGame
import qualified Controllers.Game.Model.ServerPlayer as SP
import Controllers.User.Model.ServerUser (ServerUser (ServerUser))
import Data.Conduit
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Time
import GHC.IO
import Controllers.User.UserController (UserController)
import qualified Controllers.User.UserController as UC
import qualified Controllers.User.Model.ServerUser as SU
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
import qualified Modules.Games.Api as Games
import Modules.Notifications.Api (NotificationService, sendMoveNotification, sendGameOverNotification, sendGameStartedNotification)
import Modules.UserEvent.Api (UserEventService, notifyMove, notifyGameOver, notifyPlayerActivityChanged)
import Wordify.Rules.Board (textRepresentation)

handlePlayerConnect :: Games.GameService -> UserEventService -> ServerGame -> Maybe ServerUser -> IO ()
handlePlayerConnect _ _ _ Nothing = pure ()
handlePlayerConnect gameService userEventSvc serverGame (Just user) = do
  now <- getCurrentTime
  Games.updatePlayerLastSeen gameService (gameId serverGame) user now
  atomically $ do
    newConnectionCount <- increasePlayerConnections serverGame user now
    when (isFirstConnection newConnectionCount) $ do
      notifyPlayerConnect serverGame user now
      notifyUserChannelsPlayerActivity userEventSvc serverGame

  where
    isFirstConnection :: Maybe Int -> Bool
    isFirstConnection (Just connectionCount) = connectionCount == 1
    isFirstConnection Nothing = False

handlePlayerDisconnect :: Games.GameService -> UserEventService -> ServerGame -> Maybe ServerUser -> IO ()
handlePlayerDisconnect _ _ _ Nothing = pure ()
handlePlayerDisconnect gameService userEventSvc serverGame (Just user) = do
  now <- getCurrentTime
  Games.updatePlayerLastSeen gameService (gameId serverGame) user now
  atomically $ do
    newConnectionCount <- decreasePlayerConnections serverGame user now
    when (isLastConnection newConnectionCount) $ do
      notifyPlayerDisconnect serverGame user now
      notifyUserChannelsPlayerActivity userEventSvc serverGame

  where
    isLastConnection :: Maybe Int -> Bool
    isLastConnection (Just connectionCount) = connectionCount == 0
    isLastConnection Nothing = False

notifyUserChannelsPlayerActivity :: UserEventService -> ServerGame -> STM ()
notifyUserChannelsPlayerActivity userEventSvc serverGame = do
  players <- mapM (readTVar . snd) (playing serverGame)
  let activeNames = [ defaultPlayerName i p | (i, p) <- Prelude.zip [1..] players, SP.numConnections p > 0 ]
      gId = gameId serverGame
  forM_ players $ \player -> do
    let userIdent = SP.playerId player
    notifyPlayerActivityChanged userEventSvc userIdent gId activeNames

notifyPlayerConnect :: ServerGame -> ServerUser -> UTCTime -> STM ()
notifyPlayerConnect serverGame user now = do
  let playerNumber = getPlayerNumber serverGame user
  traverse_ (writeTChan (broadcastChannel serverGame) . flip PlayerConnect now) playerNumber

notifyPlayerDisconnect :: ServerGame -> ServerUser -> UTCTime -> STM ()
notifyPlayerDisconnect serverGame user now = do
  let playerNumber = getPlayerNumber serverGame user
  traverse_ (writeTChan (broadcastChannel serverGame) . flip PlayerDisconnect now) playerNumber

withNotifyJoinAndLeave :: Games.GameService -> UserEventService -> ServerGame -> Maybe ServerUser -> IO () -> IO ()
withNotifyJoinAndLeave gameService userEventSvc serverGame maybeUser = bracket_ (handlePlayerConnect gameService userEventSvc serverGame maybeUser) (handlePlayerDisconnect gameService userEventSvc serverGame maybeUser)

performRequest :: ServerGame -> Chatroom -> GameDefinitionController -> Games.GameService -> UserEventService -> NotificationService -> UserController -> Maybe ServerUser -> ClientMessage -> IO ServerResponse
performRequest serverGame _ _ gameService userEventSvc pushCtrl _ player (BoardMove placed) =
  handleBoardMove serverGame gameService userEventSvc pushCtrl (player >>= getPlayerNumber serverGame) placed
performRequest serverGame _ _ gameService userEventSvc pushCtrl _ player (ExchangeMove exchanged) =
  handleExchangeMove serverGame gameService userEventSvc pushCtrl (player >>= getPlayerNumber serverGame) exchanged
performRequest serverGame _ _ gameService userEventSvc pushCtrl _ player PassMove =
  handlePassMove serverGame gameService userEventSvc pushCtrl (player >>= getPlayerNumber serverGame)
performRequest serverGame chatroom _ _ _ _ userCtrl player (SendChatMessage msg) =
  handleChatMessage serverGame chatroom userCtrl player msg
performRequest serverGame _ _ _ _ _ _ player (AskPotentialScore placed) =
  handlePotentialScore serverGame placed
performRequest serverGame _ gameDefinitionWorker _ userEventSvc _ _ player (AskDefinition word) =
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
  Games.GameService ->
  UserEventService ->
  NotificationService ->
  Int ->
  Move ->
  (Either Text GameTransition -> ServerResponse) ->
  IO ServerResponse
handleMove serverGame gameService userEventSvc pushCtrl playerMoving move moveOutcomeHandler =
  do
    now <- getCurrentTime
    moveResult <- atomically $ do
      currentGameState <- readTVar (game serverGame)
      if playerNumber currentGameState /= playerMoving
        then return $ Left "Not your move"
        else do
          let moveOutcome = A.left (pack . Prelude.show) (makeMove currentGameState move)
          let moveWithLocalisedRulesOutcome = moveOutcome >>= flip applyLocalisedRules (gameSetup serverGame)

          case moveWithLocalisedRulesOutcome of
            Left err -> return $ Left err
            Right transition -> do
              let eventMessage = transitionToMessage transition
              let newGameState = newGame transition
              let channel = broadcastChannel serverGame

              writeTVar (game serverGame) newGameState
              updateLastMoveMade serverGame now
              writeTChan channel eventMessage
              snapshot <- makeServerGameSnapshot serverGame

              return $ Right (moveOutcome, transition, newGameState, eventMessage, snapshot)

    case moveResult of
      Left err -> return $ moveOutcomeHandler $ Left err
      Right (moveOutcome, transition, newGameState, eventMessage, newSnapshot) -> do
        let isGameOver = case transition of
              GameFinished _ _ -> True
              _ -> False

        updateUserChannelsOfMove userEventSvc serverGame newSnapshot isGameOver
        sendMovePushNotification pushCtrl newSnapshot transition (currentPlayerToMove newSnapshot)

        -- TODO: Think of how to handle this if it fails... Could end up with a corrupted game state if next move succeeds (missing move)
        -- especially important if we stop using sqlite ':D
        forM_ (toMoveUpdate (snapshotGameId newSnapshot) newGameState eventMessage) (Games.saveMove gameService)

        return $ moveOutcomeHandler moveOutcome

sendMovePushNotification :: NotificationService -> ServerGameSnapshot -> GameTransition -> Maybe Text -> IO ()
sendMovePushNotification pushCtrl snapshot (GameFinished _ _) _ =
  forM_ (snapshotPlayers snapshot) $ \player ->
    sendGameOverNotification pushCtrl (SP.playerId player) (snapshotGameId snapshot)
sendMovePushNotification _ _ _ Nothing = pure ()
sendMovePushNotification pushCtrl snapshot _ (Just nextPlayerId) =
  sendMoveNotification pushCtrl nextPlayerId (snapshotGameId snapshot)

updateUserChannelsOfMove :: UserEventService -> ServerGame -> ServerGameSnapshot -> Bool -> IO ()
updateUserChannelsOfMove userEventSvc serverGame snapshot gameOver = do
  let players = snapshotPlayers snapshot
      gId = snapshotGameId snapshot
  forM_ players $ \player -> do
    let userIdent = SP.playerId player
    atomically $
      if gameOver
        then notifyGameOver userEventSvc userIdent gId serverGame
        else notifyMove userEventSvc userIdent gId serverGame

applyLocalisedRules :: GameTransition -> LocalisedGameSetup -> Either Text GameTransition
applyLocalisedRules gameTransition localisedGameSetup =
  let extraLocalisationGameRules = extraRules localisedGameSetup
  in let ruleApplicationResult = applyExtraRules gameTransition extraLocalisationGameRules
  in let resultWithTransformedError = A.left (pack . Prelude.show) ruleApplicationResult
  in finalTransition <$> resultWithTransformedError

handleBoardMove :: ServerGame -> Games.GameService -> UserEventService -> NotificationService -> Maybe Int -> [(Pos, Tile)] -> IO ServerResponse
handleBoardMove _ _ _ _ Nothing _ = return $ InvalidCommand "Observers cannot move"
handleBoardMove sharedServerGame gameService userEventSvc pushCtrl (Just playerNo) placed =
  handleMove
    sharedServerGame
    gameService
    userEventSvc
    pushCtrl
    playerNo
    (PlaceTiles $ M.fromList placed)
    moveOutcomeHandler
  where
    moveOutcomeHandler (Left err) = InvalidCommand err
    moveOutcomeHandler (Right (MoveTransition newPlayer _ _)) = BoardMoveSuccess (tilesOnRack newPlayer)
    moveOutcomeHandler (Right _) = BoardMoveSuccess []

handleExchangeMove :: ServerGame -> Games.GameService -> UserEventService -> NotificationService -> Maybe Int -> [Tile] -> IO ServerResponse
handleExchangeMove _ _ _ _ Nothing _ = return $ InvalidCommand "Observers cannot move"
handleExchangeMove sharedServerGame gameService userEventSvc pushCtrl (Just playerNo) exchanged =
  handleMove
    sharedServerGame
    gameService
    userEventSvc
    pushCtrl
    playerNo
    (Exchange exchanged)
    moveOutcomeHandler
  where
    moveOutcomeHandler (Left err) = InvalidCommand err
    moveOutcomeHandler (Right (ExchangeTransition _ _ afterPlayer _)) = ExchangeMoveSuccess (tilesOnRack afterPlayer)
    moveOutcomeHandler _ = InvalidCommand "internal server error, unexpected transition"

handlePassMove :: ServerGame -> Games.GameService -> UserEventService -> NotificationService -> Maybe Int -> IO ServerResponse
handlePassMove _ _ _ _ Nothing = return $ InvalidCommand "Observers cannot move"
handlePassMove sharedServerGame gameService userEventSvc pushCtrl (Just playerNo) =
  handleMove sharedServerGame gameService userEventSvc pushCtrl playerNo Pass moveOutcomeHandler
  where
    moveOutcomeHandler (Left err) = InvalidCommand err
    moveOutcomeHandler (Right _) = PassMoveSuccess

toMoveUpdate :: Text -> Game -> GameMessage -> Maybe Games.MoveUpdate
toMoveUpdate gId gameState (PlayerBoardMove _ placed _ _ _ _) =
  Just $ Games.BoardMoveUpdate gId moveNum placed boardText
  where
    moveNum = L.length (movesMade gameState)
    boardText = pack $ textRepresentation (board gameState)
toMoveUpdate gId gameState (PlayerPassMove _ _ _) =
  Just $ Games.PassMoveUpdate gId moveNum boardText
  where
    moveNum = L.length (movesMade gameState)
    boardText = pack $ textRepresentation (board gameState)
toMoveUpdate gId gameState (PlayerExchangeMove _ _ tiles _) =
  Just $ Games.ExchangeMoveUpdate gId moveNum tiles boardText
  where
    moveNum = L.length (movesMade gameState)
    boardText = pack $ textRepresentation (board gameState)
toMoveUpdate gId gameState (GameEnd _ placed _) =
  Just $ Games.GameEndMoveUpdate gId moveNum placed boardText
  where
    moveNum = L.length (movesMade gameState)
    boardText = pack $ textRepresentation (board gameState)
toMoveUpdate _ _ _ = Nothing

handleChatMessage :: ServerGame -> Chatroom -> UserController -> Maybe ServerUser -> Text -> IO ServerResponse
handleChatMessage _ _ _ Nothing _ = return $ InvalidCommand "Only logged in players can chat."
handleChatMessage serverGame chatroom userCtrl (Just user) messageText = do
  gameSnapshot <- atomically $ makeServerGameSnapshot serverGame
  let userId = SU.userId user
  displayName <- case getServerPlayerSnapshot gameSnapshot user of
    Just serverPlayer -> pure $ SP.playerUsername serverPlayer
    Nothing           -> fmap (>>= SU.username) (UC.getUser userCtrl userId)
  case displayName of
    Nothing   -> return $ InvalidCommand "Internal server error"
    Just name -> sendMessage chatroom (CR.SendMessage userId name messageText) >> return ChatSuccess

defaultPlayerName :: Int -> SP.ServerPlayer -> Text
defaultPlayerName n player = fromMaybe (pack ("Player " ++ Prelude.show n)) (SP.playerUsername player)
