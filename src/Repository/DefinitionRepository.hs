module Repository.DefinitionRepository
  ( toDefinitionRepositoryImpl,
    DefinitionRepository (getDefinitions, saveGameDefinitions, getGameDefinitions, countGameDefinitions),
    DefinitionRepositoryImpl,
    getWordDefinitionsImpl,
    saveGameDefinitionsImpl,
    getGameDefinitionsImpl,
    countGameWordDefinitionsImpl,
    GameWordItem (GameWordItem),
    WordDefinitionItem (WordDefinitionItem),
  )
where

import ClassyPrelude (IO, Int, Maybe, UTCTime)
import Conduit (ConduitT)
import qualified Data.Text as T

data WordDefinitionItem = WordDefinitionItem {partOfSpeech :: T.Text, definition :: T.Text, example :: Maybe T.Text}

data GameWordItem = GameWordItem {word :: T.Text, when :: UTCTime, definitions :: [WordDefinitionItem], definitionNumber :: Int}

class DefinitionRepository a where
  getDefinitions :: a -> T.Text -> IO [WordDefinitionItem]
  saveGameDefinitions :: a -> UTCTime -> T.Text -> T.Text -> [WordDefinitionItem] -> IO ()
  getGameDefinitions :: a -> T.Text -> ConduitT () GameWordItem IO ()
  countGameDefinitions :: a -> T.Text -> IO Int

data DefinitionRepositoryImpl = DefinitionRepositoryImpl
  { getWordDefinitions :: T.Text -> IO [WordDefinitionItem],
    saveGameWordDefinitions :: UTCTime -> T.Text -> T.Text -> [WordDefinitionItem] -> IO (),
    getGameWordDefinitions :: T.Text -> ConduitT () GameWordItem IO (),
    countGameWordDefinitions :: T.Text -> IO Int
  }

toDefinitionRepositoryImpl :: (DefinitionRepository a) => a -> DefinitionRepositoryImpl
toDefinitionRepositoryImpl repository =
  DefinitionRepositoryImpl
    { getWordDefinitions = getDefinitions repository,
      saveGameWordDefinitions = saveGameDefinitions repository,
      getGameWordDefinitions = getGameDefinitions repository,
      countGameWordDefinitions = countGameDefinitions repository
    }

getWordDefinitionsImpl :: DefinitionRepositoryImpl -> T.Text -> IO [WordDefinitionItem]
getWordDefinitionsImpl
  (DefinitionRepositoryImpl getWordDefinitions _ _ _) =
    getWordDefinitions

saveGameDefinitionsImpl :: DefinitionRepositoryImpl -> UTCTime -> T.Text -> T.Text -> [WordDefinitionItem] -> IO ()
saveGameDefinitionsImpl
  (DefinitionRepositoryImpl _ saveGameWordDefinitions _ _) =
    saveGameWordDefinitions

getGameDefinitionsImpl :: DefinitionRepositoryImpl -> T.Text -> ConduitT () GameWordItem IO ()
getGameDefinitionsImpl
  (DefinitionRepositoryImpl _ _ getGameWordDefinitions _) =
    getGameWordDefinitions

countGameWordDefinitionsImpl :: DefinitionRepositoryImpl -> T.Text -> IO Int
countGameWordDefinitionsImpl
  (DefinitionRepositoryImpl _ _ _ countGameWordDefinitions) =
    countGameWordDefinitions