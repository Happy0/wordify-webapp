module Controllers.Definition.DefinitionService (DefinitionService (getDefinitions), Definition (Definition), DefinitionServiceImpl (DefinitionServiceImpl), definitions, toDefinitionServiceImpl, withDefinitionsAsync, getDefinitionsImpl) where

import ClassyPrelude (Either (Left), IO, Int, Maybe, Show, either, id, pure, void, ($), (*), (.), (>>), (>>=))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race)
import qualified Data.Text as T

data Definition = Definition {partOfSpeech :: T.Text, definition :: T.Text, example :: Maybe T.Text} deriving (Show)

class DefinitionService a where
  getDefinitions :: a -> T.Text -> T.Text -> IO (Either T.Text [Definition])

data DefinitionServiceImpl = DefinitionServiceImpl
  { definitions :: T.Text -> T.Text -> IO (Either T.Text [Definition])
  }

toDefinitionServiceImpl :: (DefinitionService a) => a -> DefinitionServiceImpl
toDefinitionServiceImpl service = DefinitionServiceImpl (getDefinitions service)

withDefinitionsAsync :: DefinitionServiceImpl -> T.Text -> T.Text -> Int -> (Either T.Text [Definition] -> IO ()) -> IO ()
withDefinitionsAsync definitionService word localeShortcode timeoutSeconds withDefinitionsAction = void $
  forkIO $
    do
      definitionOrError <- getDefinitionOrTimeout definitionService word localeShortcode timeoutSeconds
      withDefinitionsAction definitionOrError

getDefinitionOrTimeout :: DefinitionServiceImpl -> T.Text -> T.Text -> Int -> IO (Either T.Text [Definition])
getDefinitionOrTimeout (DefinitionServiceImpl getDefinitions) word localeShortcode timeoutAfter = do
  race (timeout timeoutAfter) (getDefinitions word localeShortcode) >>= pure . either id id
  where
    timeout :: Int -> IO (Either T.Text [Definition])
    timeout timeoutAfter = threadDelay (1000000 * timeoutAfter) >> pure (Left "Timed out while getting definition.")

getDefinitionsImpl :: DefinitionServiceImpl -> T.Text -> T.Text -> IO (Either T.Text [Definition])
getDefinitionsImpl (DefinitionServiceImpl getDefinitions) word localeShortcode = getDefinitions word localeShortcode