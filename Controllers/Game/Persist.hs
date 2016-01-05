module Controllers.Game.Persist (getChatMessages, getGame, persistNewGame) where

    import Prelude
    import Control.Concurrent
    import Control.Monad
    import Control.Monad.IO.Class
    import Control.Concurrent.STM
    import Control.Concurrent.STM.TChan
    import Control.Monad.Trans.Either
    import Controllers.Game.Api
    import Controllers.Game.Model.ServerGame
    import Controllers.Game.Model.ServerPlayer
    import Control.Error.Util
    import Data.Conduit
    import qualified Data.Conduit.List as CL
    import qualified Data.Char as C
    import qualified Data.List as L
    import qualified Data.Map as Mp
    import qualified Data.Text as T
    import Data.Pool
    import Database.Persist.Sql
    import Data.Text
    import Data.Time.Clock
    import qualified Model as M
    import System.Random
    import Wordify.Rules.Board
    import Wordify.Rules.Dictionary
    import Wordify.Rules.LetterBag
    import Wordify.Rules.Move
    import Wordify.Rules.Pos
    import Wordify.Rules.Tile
    import Wordify.Rules.Game

    getChatMessages gameId =
                 selectSource [M.ChatMessageGame ==. gameId] [Asc M.ChatMessageCreatedAt]
                    $= chatMessageFromEntity

    getGame :: Pool SqlBackend -> LetterBag -> Dictionary -> Text -> IO (Either Text (TVar ServerGame))
    getGame pool bag dictionary gameId = do
        dbEntries <- withPool pool $ do
            maybeGame <- selectFirst [M.GameGameId ==. gameId] []
            case maybeGame of
                Nothing -> return $ Left (T.concat ["Game with id ", gameId, " does not exist"])
                Just gameModel -> do
                    players <- selectList [M.PlayerGame ==. gameId] []
                    moves <- selectList [M.MoveGame ==. gameId] []
                    return $ Right (gameModel, players, L.map entityVal moves)

        case dbEntries of
            Right (Entity _ (M.Game _ bagText bagSeed), playerModels, moveModels) -> do
                let serverPlayers = L.map playerFromEntity playerModels

                runEitherT $ do
                    internalPlayers <- hoistEither $ makeGameStatePlayers (L.length playerModels)
                    tiles <- hoistEither $ dbTileRepresentationToTiles bag bagText
                    let bag = makeBagUsingGenerator tiles (read (unpack bagSeed) :: StdGen)
                    game <- hoistEither $ mapLeft (pack . show) (makeGame internalPlayers bag dictionary)

                    currentGame <- hoistEither $ playThroughGame game bag moveModels

                    channel <- liftIO $ newBroadcastTChanIO
                    liftIO $ newTVarIO $ ServerGame currentGame serverPlayers channel

            Left err -> return $ Left err

    playThroughGame :: Game -> LetterBag -> [M.Move] -> Either Text Game
    playThroughGame game initialBag moves = foldM playNextMove game moves
        where
            playNextMove :: Game -> M.Move -> Either Text Game
            playNextMove game moveModel =
                case moveFromEntity (board game) initialBag moveModel of
                    Left err ->  Left err
                    Right move ->
                        let moveResult = makeMove game move
                        in case moveResult of
                            Left invalidState -> Left $ pack . show $ invalidState
                            Right moveResult -> Right (newGame moveResult)

            scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
            scanM f q [] = return [q]
            scanM f q (x:xs) =
                do
                    q2 <- f q x
                    qs <- scanM f q2 xs
                    return (q:qs)

    mapLeft :: (a -> c) -> Either a b -> Either c b
    mapLeft func (Left err) = Left $ func err
    mapLeft _ (Right r) = Right r

                    {-
        Persists the original game state (before the game has begun) and
        then listens for game events and updates the game in storage as
        it is played
    -}
    persistNewGame :: Pool SqlBackend -> Text -> ServerGame -> TChan GameMessage -> IO ()
    persistNewGame pool gameId serverGame messageChannel = do
        forkIO $ do
             key <- persistGameState pool gameId serverGame
             watchForUpdates pool gameId  messageChannel
             return ()

        return ()

    withPool pool = flip runSqlPersistMPool pool

    persistGameState :: Pool SqlBackend -> Text -> ServerGame -> IO (Key M.Game)
    persistGameState pool gameId serverGame = do
        withPool pool $ do
            gameDbId <- insert $
                (M.Game
                    gameId
                    (tilesToDbRepresentation (tiles letterBag))
                    (pack $ show (getGenerator letterBag)))

            persistPlayers gameId (playing serverGame)
            return gameDbId
        where
                gameState = game serverGame
                History letterBag _ = history gameState

    persistPlayers gameId players =
        let playersWithNumbers = L.zip [1 .. 4] players
        in flip mapM_ playersWithNumbers $ do
                    \(playerNumber, (ServerPlayer playerName identifier)) ->
                        insert $
                            M.Player gameId playerName identifier playerNumber

    watchForUpdates :: Pool SqlBackend -> Text -> TChan GameMessage -> IO ()
    watchForUpdates pool gameId messageChannel =
        forever $ (atomically . readTChan) messageChannel >>= persistUpdate pool gameId

    persistUpdate :: Pool SqlBackend -> Text -> GameMessage -> IO ()
    persistUpdate pool gameId (PlayerChat chatMessage) = persistChatMessage pool gameId chatMessage
    persistUpdate pool gameId (PlayerBoardMove moveNumber placed _ _ _ _) =
        persistBoardMove pool gameId moveNumber placed
    persistUpdate pool gameId (PlayerPassMove moveNumber _ _) = persistPassMove pool gameId moveNumber
    persistUpdate pool gameId (PlayerExchangeMove moveNumber _ exchanged _) =
        persistExchangeMove pool gameId moveNumber exchanged
    persistUpdate _ _ _ = return ()

    persistBoardMove :: Pool SqlBackend -> Text -> Int -> [(Pos, Tile)] -> IO ()
    persistBoardMove pool gameId moveNumber placed = do
        let move = M.Move
                        gameId
                        moveNumber
                        (Just $ tilesToDbRepresentation (Prelude.map snd placedSorted))
                        (Just $ xPos min)
                        (Just $ yPos min)
                        (Just isHorizontal)

        withPool pool $ do
            insert move
            return ()
        where
            placedSorted = L.sort placed
            positions = L.map fst placedSorted
            (min, max) = (L.minimum positions, L.maximum positions)
            isHorizontal = (yPos min == yPos max)

    persistPassMove :: Pool SqlBackend -> Text -> Int -> IO ()
    persistPassMove pool gameId moveNumber =
        withPool pool $ do
            insert (M.Move gameId moveNumber Nothing Nothing Nothing Nothing)
            return ()

    persistExchangeMove :: Pool SqlBackend -> Text -> Int -> [Tile] -> IO ()
    persistExchangeMove pool gameId moveNumber tiles =
        withPool pool $ do
            insert (M.Move gameId moveNumber (Just (tilesToDbRepresentation tiles)) Nothing Nothing Nothing)
            return ()

    persistChatMessage :: Pool SqlBackend -> Text -> ChatMessage -> IO ()
    persistChatMessage pool gameId (ChatMessage user message) = do
        time <- getCurrentTime
        withPool pool $ do
            insert (M.ChatMessage gameId time user message)
            return ()

    tilesToDbRepresentation :: [Tile] -> Text
    tilesToDbRepresentation tiles = pack $ Prelude.map tileToDbRepresentation tiles

    tileToDbRepresentation :: Tile -> Char
    tileToDbRepresentation (Letter lettr val) = lettr
    tileToDbRepresentation (Blank Nothing) = '_'
    tileToDbRepresentation (Blank (Just letter)) = C.toLower letter

    chatMessageFromEntity :: Monad m => Conduit (Entity M.ChatMessage) m GameMessage
    chatMessageFromEntity = CL.map fromEntity
        where
            fromEntity (Entity _ (M.ChatMessage _ created user message)) = PlayerChat (ChatMessage user message)

    moveFromEntity :: Board -> LetterBag -> M.Move -> Either Text Move
    moveFromEntity board letterBag (M.Move gameId moveNumber Nothing Nothing Nothing Nothing) = Right Pass
    moveFromEntity board letterBag (M.Move gameId moveNumber (Just tiles) Nothing Nothing Nothing) =
        case dbTileRepresentationToTiles letterBag tiles of
            Left err -> error $ "you've dun goofed... " ++ show err
            Right tiles -> Right $ Exchange tiles
    moveFromEntity board letterBag (M.Move
                     gameId
                     moveNumber
                     (Just tiles)
                     (Just startx)
                     (Just starty)
                     (Just isHorizontal)) =
            do
                let direction = if isHorizontal then Horizontal else Vertical
                let startPos = posAt (startx, starty)

                positions <- case startPos of
                    Nothing -> Left $ "u dun goofed. Start position stored for the move is invalid "
                    Just pos -> Right $ emptySquaresFrom board pos (T.length tiles) direction
                tiles <- dbTileRepresentationToTiles letterBag tiles
                return $ PlaceTiles (Mp.fromList (L.zip positions tiles))
    moveFromEntity _ _ (m@M.Move {})  = error $ "you've dun goofed, see database logs (hopefully) "

    playerFromEntity :: Entity M.Player -> ServerPlayer
    playerFromEntity (Entity _ (M.Player gameId name playerId _)) = makeNewPlayer name playerId

    dbTileRepresentationToTiles :: LetterBag -> Text -> Either Text [Tile]
    dbTileRepresentationToTiles letterBag textRepresentation =
            sequence $ fmap getTile (unpack textRepresentation)
        where
            letterMap = bagLetters letterBag
            getTile :: Char -> Either Text Tile
            getTile character
                | (C.isLower character) =  Right $ Blank (Just $ C.toUpper character)
                | character == '_' =  Right $ Blank Nothing
                | otherwise = case Mp.lookup character letterMap of
                                Just tile -> Right tile
                                _ -> Left $ pack $ (show character) ++ " not found in letterbag"
