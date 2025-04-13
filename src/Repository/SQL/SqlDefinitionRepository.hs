
module Repository.SQL.SqlDefinitionRepository where 

    import Conduit (ConduitT)
    import Data.Pool
    import qualified Data.Text as T
    import ClassyPrelude (IO, undefined)
    import Database.Persist.Sql
    import Repository.DefinitionRepository (GameWordItem, DefinitionRepository(getDefinitions, saveGameDefinitions,getGameDefinitions), WordDefinitionItem)

    data DefinitionRepositorySQLBackend = DefinitionRepositorySQLBackend (Pool SqlBackend)

    instance DefinitionRepository DefinitionRepositorySQLBackend where
        getDefinitions (DefinitionRepositorySQLBackend pool)  word = undefined
        saveGameDefinitions (DefinitionRepositorySQLBackend pool) gameId word definitions = undefined
        getGameDefinitions (DefinitionRepositorySQLBackend pool) gameId = undefined

    getDefinitionsImpl :: Pool SqlBackend -> IO [WordDefinitionItem]
    getDefinitionsImpl pool = undefined

    saveGameDefinitions :: Pool SqlBackend -> T.Text -> T.Text -> IO ()
    saveGameDefinitions pool gameId word = undefined

    getGameDefinitions :: Pool SqlBackend -> T.Text -> ConduitT () GameWordItem IO ()
    getGameDefinitions pool gameId = undefined