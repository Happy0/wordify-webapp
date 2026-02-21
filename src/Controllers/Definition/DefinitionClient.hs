module Controllers.Definition.DefinitionClient (DefinitionClient (getDefinitions, supportedLocales), Definition (Definition)) where

import ClassyPrelude (Either, IO, Maybe, Show)
import qualified Data.Text as T

data Definition = Definition {partOfSpeech :: T.Text, definition :: T.Text, example :: Maybe T.Text} deriving (Show)

class DefinitionClient a where
  getDefinitions :: a -> T.Text -> T.Text -> IO (Either T.Text [Definition])
  supportedLocales :: a -> [T.Text]
