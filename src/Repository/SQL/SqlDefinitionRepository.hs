
module Repository.SQL.SqlDefinitionRepository where 

    import Conduit (ConduitT)
    import Data.Pool

    data DefinitionRepositorySQLBackend = DefinitionRepositorySQLBackend (Pool SqlBackend)

    

