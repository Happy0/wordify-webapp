module Controllers.Definition.DefinitionService (DefinitionService(getDefinitions), Definition(Definition), DefinitionServiceImpl(DefinitionServiceImpl), definitions, toDefinitionServiceImpl) where
    import qualified Data.Text as T
    import ClassyPrelude (IO, Either, Show, Maybe)

    data Definition = Definition {partOfSpeech :: T.Text, definition :: T.Text, example :: Maybe T.Text} deriving Show

    class DefinitionService a where
        getDefinitions :: a -> T.Text -> IO (Either T.Text [Definition])

    data DefinitionServiceImpl = DefinitionServiceImpl {
        definitions :: T.Text -> IO (Either T.Text [Definition])
    }

    toDefinitionServiceImpl :: DefinitionService a => a -> DefinitionServiceImpl
    toDefinitionServiceImpl service = DefinitionServiceImpl (getDefinitions service)