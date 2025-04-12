module Controllers.Definition.DefinitionService (DefinitionService(getDefinitions), Definition(Definition), DefinitionServiceImpl(DefinitionServiceImpl), definitions, toDefinitionServiceImpl) where
    import qualified Data.Text as T
    import ClassyPrelude (IO, Either, Show, Maybe, void, Int, pure, (>>), (>>=), Either(Left, Right), ($), (*), join, (<$>), either, id, (.))
    import Control.Concurrent.Async (race)
    import Control.Concurrent (forkIO, threadDelay)

    data Definition = Definition {partOfSpeech :: T.Text, definition :: T.Text, example :: Maybe T.Text} deriving Show

    class DefinitionService a where
        getDefinitions :: a -> T.Text -> IO (Either T.Text [Definition])

    data DefinitionServiceImpl = DefinitionServiceImpl {
        definitions :: T.Text -> IO (Either T.Text [Definition])
    }

    toDefinitionServiceImpl :: DefinitionService a => a -> DefinitionServiceImpl
    toDefinitionServiceImpl service = DefinitionServiceImpl (getDefinitions service)

    withDefinitionsAsync :: DefinitionServiceImpl -> T.Text -> Int -> (Either T.Text [Definition] -> IO ()) -> IO ()
    withDefinitionsAsync definitionService word timeoutSeconds withDefinitionsAction = void $ forkIO $
        do 
            definitionOrError <- getDefinitionOrTimeout definitionService word timeoutSeconds
            withDefinitionsAction definitionOrError

    getDefinitionOrTimeout :: DefinitionServiceImpl -> T.Text -> Int -> IO (Either T.Text [Definition])
    getDefinitionOrTimeout (DefinitionServiceImpl getDefinitions) word timeoutAfter = do 
        race (timeout timeoutAfter) (getDefinitions word) >>= pure . either id id
        where 
            timeout :: Int -> IO (Either T.Text [Definition])
            timeout timeoutAfter =  threadDelay (1000000 * timeoutAfter) >> pure (Left "Timed out while getting definition.")