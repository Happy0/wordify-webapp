{-# LANGUAGE ExistentialQuantification #-}
module Controllers.Definition.DefinitionService (DefinitionService, AnyDefinitionClient, anyDefinitionClient, makeDefinitionService, getDefinition) where

import ClassyPrelude (Either (Right), IO, Maybe (Just, Nothing), elem, map, pure, (.))
import Controllers.Definition.DefinitionClient (Definition, DefinitionClient, getDefinitions, supportedLocales)
import Data.List (find)
import qualified Data.Text as T

data AnyDefinitionClient = forall a. DefinitionClient a => AnyDefinitionClient a

anyDefinitionClient :: DefinitionClient a => a -> AnyDefinitionClient
anyDefinitionClient = AnyDefinitionClient

newtype DefinitionService = DefinitionService [AnyDefinitionClient]

makeDefinitionService :: [AnyDefinitionClient] -> DefinitionService
makeDefinitionService = DefinitionService

getDefinition :: DefinitionService -> T.Text -> T.Text -> IO (Either T.Text [Definition])
getDefinition (DefinitionService clients) word locale =
  case find supportsLocale clients of
    Nothing -> pure (Right [])
    Just (AnyDefinitionClient client) -> getDefinitions client word locale
  where
    supportsLocale (AnyDefinitionClient client) = locale `elem` supportedLocales client
