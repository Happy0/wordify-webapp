module Controllers.Game.Persist (getChatMessages, persistNewGame) where

    import Prelude
    import Control.Concurrent
    import Control.Monad
    import Control.Monad.IO.Class
    import Control.Concurrent.STM
    import Control.Concurrent.STM.TChan
    import Controllers.Game.Api
    import Controllers.Game.Model.ServerGame
    import Controllers.Game.Model.ServerPlayer
    import Data.Conduit
    import qualified Data.Conduit.List as CL
    import qualified Data.Char as C
    import qualified Data.List as L
    import qualified Data.Map as Mp
    import Data.Pool
    import Database.Persist.Sql
    import Data.Text
    import Data.Time.Clock
    import qualified Model as M
    import Wordify.Rules.LetterBag
    import Wordify.Rules.Move
    import Wordify.Rules.Pos
    import Wordify.Rules.Tile
    import Wordify.Rules.Game

    chatMessageFromEntity :: Monad m => Conduit (Entity M.ChatMessage) m GameMessage
    chatMessageFromEntity = CL.map fromEntity
        where
            fromEntity (Entity _ (M.ChatMessage _ created user message)) = PlayerChat (ChatMessage user message)

    getChatMessages gameId =
                 selectSource [M.ChatMessageGame ==. gameId] [Asc M.ChatMessageCreatedAt]
                    $= chatMessageFromEntity

    moveFromEntity :: LetterBag -> M.Move -> Move
    moveFromEntity letterBag (M.Move gameId moveNumber Nothing Nothing Nothing Nothing) = Pass
    moveFromEntity letterBag (M.Move gameId moveNumber (Just tiles) Nothing Nothing Nothing) =
        case dbTileRepresentationToTiles letterBag tiles of
            Left err -> error $ "you've dun goofed... " ++ show err
            Right tiles -> Exchange tiles
    moveFromEntity _ (M.Move
                     gameId
                     moveNumber
                     (Just tiles)
                     (Just startx)
                     (Just starty)
                     (Just isHorizontal)) = undefined
    moveFromEntity _ (m@M.Move {})  = error $ "you've dun goofed, see database logs (hopefully) "

    dbTileRepresentationToTiles :: LetterBag -> Text -> Either Text [Tile]
    dbTileRepresentationToTiles letterBag textRepresentation =
            sequence $ fmap getTile (show textRepresentation)
        where
            letterMap = bagLetters letterBag
            getTile :: Char -> Either Text Tile
            getTile character
                | (C.isLower character) =  Right $ Blank (Just character)
                | character == '_' =  Right $ Blank Nothing
                | otherwise = case Mp.lookup character letterMap of
                                Just tile -> Right tile
                                _ -> Left $ pack $ (show character) ++ " not found in letterbag"

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

