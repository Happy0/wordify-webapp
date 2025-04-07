module Controllers.Definition.DefinitionService (DefinitionService(getDefinitions), Definition(Definition)) where
    import qualified Data.Text as T
    import ClassyPrelude (IO, Either)

    data Definition = Definition {partOfSpeach :: T.Text, definition :: T.Text}

    class DefinitionService a where
        getDefinitions :: a -> T.Text -> IO (Either T.Text [Definition])
