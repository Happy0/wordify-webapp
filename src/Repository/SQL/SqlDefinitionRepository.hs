
module Repository.SQL.SqlDefinitionRepository(DefinitionRepositorySQLBackend(DefinitionRepositorySQLBackend), getDefinitionsImpl) where 

    import Conduit (ConduitT)
    import Control.Monad.IO.Class (liftIO)
    import qualified Database.Esqueleto as E
    import Data.Pool
    import qualified Model as M
    import qualified Data.Text as T
    import qualified Data.Text.Encoding as TE
    import ClassyPrelude (IO, undefined, flip, UTCTime, (.), mapConcurrently, ($), zip, map, Maybe(Just, Nothing), pure, liftIO, mapM, return)
    import Database.Persist.Sql
    import Repository.DefinitionRepository (WordDefinitionItem(WordDefinitionItem), GameWordItem, DefinitionRepository(getDefinitions, saveGameDefinitions,getGameDefinitions), WordDefinitionItem)
    import qualified Data.ByteString.Base16 as B16
    import Data.Conduit.List (sourceList)

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
                    E.on (gameDef E.^. M.GameDefinitionDefinition E.==. def E.^. M.DefinitionId)
                    E.where_ (gameDef E.^. M.GameDefinitionGameId E.==. E.val (M.GameKey gameId))
                    return (gameDef, def)
        -- todo: stream from database using appropriate query
        sourceList (makeGameWordDefinitions defs)
                    
    makeGameWordDefinitions :: [(E.Entity M.GameDefinition, E.Entity M.Definition)] -> [GameWordItem]
    makeGameWordDefinitions definitions = undefined

    saveGameDefinitionsImpl :: Pool SqlBackend -> UTCTime -> T.Text -> T.Text -> [WordDefinitionItem]-> IO ()
    saveGameDefinitionsImpl pool when gameId word definitions = do
        -- TODO: do this in SQL with persist bulk upsert / raw SQL if necessary
        definitions <- mapM (upsertDefinition pool when word) definitions
        withPool pool $ do
            let gameDefinitions = map (makeGameDefinition when gameId word) definitions
            insertMany_ gameDefinitions
        
    makeGameDefinition :: UTCTime -> T.Text -> T.Text -> Key M.Definition -> M.GameDefinition
    makeGameDefinition createdAt gameId word definitionId = 
        M.GameDefinition word createdAt definitionId (M.GameKey gameId)

    upsertDefinition :: Pool SqlBackend -> UTCTime -> T.Text -> WordDefinitionItem -> IO (Key M.Definition)
    upsertDefinition pool when word definition@(WordDefinitionItem partOfSpeech wordDefinition _) = withPool pool $ do
        let dbModel = gameDefinitionDatabaseModel word definition
        (Entity entityKey entity) <- upsertBy (M.UniqueWordPartOfSpeechDefinition word partOfSpeech wordDefinition) dbModel []
        pure entityKey

    gameDefinitionDatabaseModel :: T.Text -> WordDefinitionItem -> M.Definition
    gameDefinitionDatabaseModel word (WordDefinitionItem partOfSpeech definition example) = 
        M.Definition word partOfSpeech definition example

    withPool pool = flip runSqlPersistMPool pool