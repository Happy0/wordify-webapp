module Repository.DefinitionRepository
  ( toDefinitionRepositoryImpl,
    DefinitionRepository (getDefinitions, saveGameDefinitions, getGameDefinitions),
    DefinitionRepositoryImpl,
    getWordDefinitionsImpl,
    saveGameDefinitionsImpl,
    getGameDefinitionsImpl,
    GameWordItem (GameWordItem),
    WordDefinitionItem (WordDefinitionItem),
  )
where

import ClassyPrelude (IO, Maybe, UTCTime)
import Conduit (ConduitT)
import qualified Data.Text as T

data WordDefinitionItem = WordDefinitionItem {partOfSpeech :: T.Text, definition :: T.Text, example :: Maybe T.Text}

data GameWordItem = GameWordItem {word :: T.Text, when :: UTCTime, definitions :: [WordDefinitionItem]}

class DefinitionRepository a where
  getDefinitions :: a -> T.Text -> IO [WordDefinitionItem]
  saveGameDefinitions :: a -> UTCTime -> T.Text -> T.Text -> [WordDefinitionItem] -> IO ()
  getGameDefinitions :: a -> T.Text -> ConduitT () GameWordItem IO ()

data DefinitionRepositoryImpl = DefinitionRepositoryImpl
  { getWordDefinitions :: T.Text -> IO [WordDefinitionItem],
    saveGameWordDefinitions :: UTCTime -> T.Text -> T.Text -> [WordDefinitionItem] -> IO (),
    getGameWordDefinitions :: T.Text -> ConduitT () GameWordItem IO ()
  }

toDefinitionRepositoryImpl :: (DefinitionRepository a) => a -> DefinitionRepositoryImpl
toDefinitionRepositoryImpl repository =
  DefinitionRepositoryImpl {getWordDefinitions = getDefinitions repository, saveGameWordDefinitions = saveGameDefinitions repository, getGameWordDefinitions = getGameDefinitions repository}

getWordDefinitionsImpl :: DefinitionRepositoryImpl -> T.Text -> IO [WordDefinitionItem]
getWordDefinitionsImpl
  (DefinitionRepositoryImpl getWordDefinitions _ _) =
    getWordDefinitions

saveGameDefinitionsImpl :: DefinitionRepositoryImpl -> UTCTime -> T.Text -> T.Text -> [WordDefinitionItem] -> IO ()
saveGameDefinitionsImpl
  (DefinitionRepositoryImpl _ saveGameWordDefinitions _) =
    saveGameWordDefinitions

getGameDefinitionsImpl :: DefinitionRepositoryImpl -> T.Text -> ConduitT () GameWordItem IO ()
getGameDefinitionsImpl
  (DefinitionRepositoryImpl _ _ getGameWordDefinitions) =
    getGameWordDefinitions