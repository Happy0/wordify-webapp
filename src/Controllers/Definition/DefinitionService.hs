module Controllers.Definition.DefinitionService (DefinitionService(getDefinitions), Definition(Definition), DefinitionServiceImpl(DefinitionServiceImpl), definitions, toDefinitionServiceImpl) where
    import qualified Data.Text as T
    import ClassyPrelude (IO, Either)

    data Definition = Definition {partOfSpeech :: T.Text, definition :: T.Text}

    class DefinitionService a where
        getDefinitions :: a -> T.Text -> IO (Either T.Text [Definition])

    data DefinitionServiceImpl = DefinitionServiceImpl {
        definitions :: T.Text -> IO (Either T.Text [Definition])
    }

    toDefinitionServiceImpl :: DefinitionService a => a -> DefinitionServiceImpl
    toDefinitionServiceImpl service = DefinitionServiceImpl (getDefinitions service)