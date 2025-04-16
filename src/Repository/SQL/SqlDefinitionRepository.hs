
module Repository.SQL.SqlDefinitionRepository(DefinitionRepositorySQLBackend(DefinitionRepositorySQLBackend), getDefinitionsImpl) where 

    import Conduit (ConduitT)
    import Control.Monad.IO.Class (liftIO)
    import qualified Database.Esqueleto as E
    import Data.Pool
    import qualified Model as M
    import qualified Data.Text as T
    import qualified Data.Text.Encoding as TE
    import ClassyPrelude (Int, IO, undefined, flip, UTCTime, (.), mapConcurrently, ($), zip, compare, map, Maybe(Just, Nothing), pure, liftIO, mapM, return, (++), fst, uncurry, Ord, zipWith, Ordering, sortBy)
    import Database.Persist.Sql
    import Repository.DefinitionRepository (WordDefinitionItem(WordDefinitionItem), GameWordItem, GameWordItem(GameWordItem), DefinitionRepository(getDefinitions, saveGameDefinitions,getGameDefinitions), WordDefinitionItem)
    import qualified Data.ByteString.Base16 as B16
    import Data.Conduit.List (sourceList)
    import qualified Data.Map.Strict as Map

    data DefinitionRepositorySQLBackend = DefinitionRepositorySQLBackend (Pool SqlBackend)

    instance DefinitionRepository DefinitionRepositorySQLBackend where
        getDefinitions (DefinitionRepositorySQLBackend pool)  word = getDefinitionsImpl pool word
        saveGameDefinitions (DefinitionRepositorySQLBackend pool) when gameId word definitions = saveGameDefinitionsImpl pool when gameId word definitions
        getGameDefinitions (DefinitionRepositorySQLBackend pool) gameId = getGameDefinitionsImpl pool gameId

    getDefinitionsImpl :: Pool SqlBackend -> T.Text -> IO [WordDefinitionItem]
    getDefinitionsImpl pool = undefined

    getGameDefinitionsImpl :: Pool SqlBackend -> T.Text -> ConduitT () GameWordItem IO ()
    getGameDefinitionsImpl pool gameId = do
        defs <- liftIO $ withPool pool $ do
            E.select $ do
                E.from $ \(gameDef `E.InnerJoin` def) -> do
                    E.on (gameDef E.^. M.GameDefinitionDefinitionId E.==. def E.^. M.DefinitionId)
                    E.where_ (gameDef E.^. M.GameDefinitionGameId E.==. E.val (M.GameKey gameId))
                    return (gameDef, def)
        -- TODO: stream from database using appropriate query rather than loading all into memory and grouping
        sourceList (makeGameWordDefinitions defs)
                    
    makeGameWordDefinitions :: [(E.Entity M.GameDefinition, E.Entity M.Definition)] -> [GameWordItem]
    makeGameWordDefinitions definitions = 
        let entities = map extractValues definitions
        in let groupedByWordAndTimestamp = groupByKey (extractWordAndTimeStamp . fst) entities
        in makeGameWorkItems groupedByWordAndTimestamp
        where
            makeGameWorkItems :: Map.Map (T.Text, UTCTime) [(M.GameDefinition, M.Definition)] -> [GameWordItem]
            makeGameWorkItems grouped = map (uncurry makeGameWordItem) (Map.toList grouped)

            makeGameWordItem :: (T.Text, UTCTime) -> [(M.GameDefinition, M.Definition)] -> GameWordItem
            makeGameWordItem (word, createdAt) definitions = 
                let wordDefinitions = map makeDefinition (sortDefinitions definitions)
                in GameWordItem word createdAt wordDefinitions
                where
                    sortDefinitions :: [(M.GameDefinition, M.Definition)] -> [(M.GameDefinition, M.Definition)]
                    sortDefinitions x = sortBy comparator x

                    comparator :: (M.GameDefinition, M.Definition) -> (M.GameDefinition, M.Definition) -> Ordering
                    comparator (gameDef1, def1) (gameDef2, def2) = compare (definitionNumber gameDef1) (definitionNumber gameDef2)

                    definitionNumber :: M.GameDefinition -> Int
                    definitionNumber (M.GameDefinition _ defNumber _ _ _) = defNumber
                        
            
            makeDefinition :: (M.GameDefinition, M.Definition) -> WordDefinitionItem
            makeDefinition (gameDef, def) = 
                let (M.Definition _ partOfSpeech definition example) = def
                in WordDefinitionItem partOfSpeech definition example

            extractValues :: (E.Entity M.GameDefinition, E.Entity M.Definition) -> (M.GameDefinition, M.Definition)
            extractValues (Entity _ gameDef, Entity _ def) = (gameDef, def)

            extractWordAndTimeStamp :: M.GameDefinition -> (T.Text, UTCTime)
            extractWordAndTimeStamp (M.GameDefinition word _ createdAt _ _) = (word, createdAt)

            groupByKey :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
            groupByKey toKey xs = Map.fromListWith (++) [(toKey x, [x]) | x <- xs]

    saveGameDefinitionsImpl :: Pool SqlBackend -> UTCTime -> T.Text -> T.Text -> [WordDefinitionItem]-> IO ()
    saveGameDefinitionsImpl pool when gameId word definitions = do
        -- TODO: do this in SQL with persist bulk upsert / raw SQL if necessary
        definitions <- mapM (upsertDefinition pool when word) definitions
        withPool pool $ do
            let gameDefinitions = zipWith (makeGameDefinition when gameId word) [1..] definitions
            insertMany_ gameDefinitions
        
    makeGameDefinition :: UTCTime -> T.Text -> T.Text -> Int -> Key M.Definition -> M.GameDefinition
    makeGameDefinition createdAt gameId word definitionNumber definitionId = 
        M.GameDefinition word definitionNumber createdAt definitionId (M.GameKey gameId)

    upsertDefinition :: Pool SqlBackend -> UTCTime -> T.Text -> WordDefinitionItem -> IO (Key M.Definition)
    upsertDefinition pool when word definition@(WordDefinitionItem partOfSpeech wordDefinition _) = withPool pool $ do
        let dbModel = gameDefinitionDatabaseModel word definition
        (Entity entityKey entity) <- upsertBy (M.UniqueWordPartOfSpeechDefinition word partOfSpeech wordDefinition) dbModel []
        pure entityKey

    gameDefinitionDatabaseModel :: T.Text -> WordDefinitionItem -> M.Definition
    gameDefinitionDatabaseModel word (WordDefinitionItem partOfSpeech definition example) = 
        M.Definition word partOfSpeech definition example

    withPool pool = flip runSqlPersistMPool pool