
module Repository.SQL.SqlDefinitionRepository where 

    import Conduit (ConduitT)
    import Data.Pool
    import qualified Data.Text as T
    import ClassyPrelude (IO, undefined)
    import Database.Persist.Sql
    import Repository.DefinitionRepository (GameWordItem, DefinitionRepository(getDefinitions, saveGameDefinitions,getGameDefinitions), WordDefinitionItem)

    data DefinitionRepositorySQLBackend = DefinitionRepositorySQLBackend (Pool SqlBackend)

    instance DefinitionRepository DefinitionRepositorySQLBackend where
        getDefinitions (DefinitionRepositorySQLBackend pool)  word = getDefinitionsImpl pool word
        saveGameDefinitions (DefinitionRepositorySQLBackend pool) gameId word definitions = saveGameDefinitionsImpl pool gameId word
        getGameDefinitions (DefinitionRepositorySQLBackend pool) gameId = getGameDefinitionsImpl pool gameId

    getDefinitionsImpl :: Pool SqlBackend -> T.Text -> IO [WordDefinitionItem]
    getDefinitionsImpl pool = undefined

    saveGameDefinitionsImpl :: Pool SqlBackend -> T.Text -> T.Text -> IO ()
    saveGameDefinitionsImpl pool gameId word = undefined

    getGameDefinitionsImpl :: Pool SqlBackend -> T.Text -> ConduitT () GameWordItem IO ()
    getGameDefinitionsImpl pool gameId = undefined