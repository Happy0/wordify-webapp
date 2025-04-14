
module Repository.SQL.SqlDefinitionRepository where 

    import Conduit (ConduitT)
    import Data.Pool
    import qualified Model as M
    import qualified Data.Text as T
    import qualified Data.Text.Encoding as TE
    import ClassyPrelude (IO, undefined, flip, UTCTime, (.), mapConcurrently, ($), zip, map, Maybe(Just, Nothing), pure, liftIO)
    import Database.Persist.Sql
    import Repository.DefinitionRepository (WordDefinitionItem(WordDefinitionItem), GameWordItem, DefinitionRepository(getDefinitions, saveGameDefinitions,getGameDefinitions), WordDefinitionItem)
    import qualified Data.ByteString.Base16 as B16

    data DefinitionRepositorySQLBackend = DefinitionRepositorySQLBackend (Pool SqlBackend)

    instance DefinitionRepository DefinitionRepositorySQLBackend where
        getDefinitions (DefinitionRepositorySQLBackend pool)  word = getDefinitionsImpl pool word
        saveGameDefinitions (DefinitionRepositorySQLBackend pool) when gameId word definitions = saveGameDefinitionsImpl pool when gameId word definitions
        getGameDefinitions (DefinitionRepositorySQLBackend pool) gameId = getGameDefinitionsImpl pool gameId

    getDefinitionsImpl :: Pool SqlBackend -> T.Text -> IO [WordDefinitionItem]
    getDefinitionsImpl pool = undefined

    getGameDefinitionsImpl :: Pool SqlBackend -> T.Text -> ConduitT () GameWordItem IO ()
    getGameDefinitionsImpl pool gameId = undefined

    saveGameDefinitionsImpl :: Pool SqlBackend -> UTCTime -> T.Text -> T.Text -> [WordDefinitionItem]-> IO ()
    saveGameDefinitionsImpl pool when gameId word definitions = do
        -- TODO: do this in SQL with persist bulk upsert / raw SQL if necessary
        definitions <- mapConcurrently (upsertDefinition pool when word) definitions
        withPool pool $ do
            maybeGame <- get (M.GameKey gameId)

            case maybeGame of
                Nothing -> pure ()
                Just gameEntity -> do
                    let gameDefinitions = map (makeGameDefinition when gameEntity word) definitions
                    putMany gameDefinitions
        
    makeGameDefinition :: UTCTime -> M.Game -> T.Text -> M.Definition -> M.GameDefinition
    makeGameDefinition createdAt game word definition = 
        M.GameDefinition word createdAt definition game

    upsertDefinition :: Pool SqlBackend -> UTCTime -> T.Text -> WordDefinitionItem -> IO M.Definition
    upsertDefinition pool when word definition@(WordDefinitionItem partOfSpeech wordDefinition _) = withPool pool $ do
        let dbModel = gameDefinitionDatabaseModel word definition
        (Entity _ entity) <- upsertBy (M.UniqueDefinition word partOfSpeech wordDefinition) dbModel []
        pure entity

    gameDefinitionDatabaseModel :: T.Text -> WordDefinitionItem -> M.Definition
    gameDefinitionDatabaseModel word (WordDefinitionItem partOfSpeech definition example) = 
        M.Definition word partOfSpeech definition example

    withPool pool = flip runSqlPersistMPool pool