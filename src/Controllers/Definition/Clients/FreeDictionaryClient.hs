module Controllers.Definition.Clients.FreeDictionaryClient (getDefinitions, FreeDictionaryClient (FreeDictionaryClient)) where

import ClassyPrelude (Either (Left, Right), IO, Maybe, Show, SomeException, String, concat, concatMap, fmap, map, mapM, mempty, pure, toList, try, undefined, ($), (.), (<$>))
import Control.Arrow (left)
import Controllers.Definition.DefinitionClient (Definition (Definition), DefinitionClient (getDefinitions, supportedLocales))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Network.HTTP.Req

data FreeDictionaryClient = FreeDictionaryClient

data FreeDictionaryDefinition = FreeDictionaryDefinition {definition :: T.Text, example :: Maybe T.Text, synonyms :: [T.Text]} deriving (Show)

data FreeDictionaryMeaning = FreeDictionaryMeaning {partOfSpeech :: T.Text, definitions :: [FreeDictionaryDefinition]} deriving (Show)

data FreeDictionaryResponseItem = FreeDictionaryResponseItem {word :: T.Text, meanings :: [FreeDictionaryMeaning]} deriving (Show)

newtype FreeDictionaryResponse = FreeDictionaryResponse [FreeDictionaryResponseItem] deriving (Show)

instance FromJSON FreeDictionaryDefinition where
  parseJSON = withObject "Definition" $ \obj -> do
    definition <- obj .: "definition"
    example <- obj .:? "example"
    synonyms <- obj .: "synonyms"
    pure $ FreeDictionaryDefinition definition example synonyms

instance FromJSON FreeDictionaryMeaning where
  parseJSON = withObject "Meaning" $ \obj -> do
    partOfSpeech <- obj .: "partOfSpeech"
    definitions <- obj .: "definitions"
    pure $ FreeDictionaryMeaning partOfSpeech definitions

instance FromJSON FreeDictionaryResponseItem where
  parseJSON = withObject "WordMeanings" $ \obj -> do
    word <- obj .: "word"
    meanings <- obj .: "meanings"
    pure $ FreeDictionaryResponseItem word meanings

instance FromJSON FreeDictionaryResponse where
  parseJSON = withArray "ResponseItems" $ \arr -> do
    items <- mapM parseJSON (toList arr)
    pure $ FreeDictionaryResponse items

instance DefinitionClient FreeDictionaryClient where
  getDefinitions = getDefinitionsImpl
  supportedLocales _ = ["en"]

getDefinitionsImpl :: FreeDictionaryClient -> T.Text -> T.Text -> IO (Either T.Text [Definition])
getDefinitionsImpl client word "en" =
  fmap freeDictionaryResponseToDefinition <$> freeDictionaryGetRequest client word
getDefinitionsImpl client word _ = pure (Right [])

freeDictionaryResponseToDefinition :: FreeDictionaryResponse -> [Definition]
freeDictionaryResponseToDefinition (FreeDictionaryResponse items) = concatMap (concatMap definitionFromFreeDictionaryMeaning . meanings) items
  where
    definitionFromFreeDictionaryMeaning :: FreeDictionaryMeaning -> [Definition]
    definitionFromFreeDictionaryMeaning (FreeDictionaryMeaning partOfSpeech definitions) = map (toDefinition partOfSpeech) definitions

    toDefinition :: T.Text -> FreeDictionaryDefinition -> Definition
    toDefinition partOfSpeech (FreeDictionaryDefinition definition example _) = Definition partOfSpeech definition example

freeDictionaryGetRequest :: FreeDictionaryClient -> T.Text -> IO (Either T.Text FreeDictionaryResponse)
freeDictionaryGetRequest freeDictionaryClient word = do
  requestResult <- try doRequest :: IO (Either SomeException (Either T.Text FreeDictionaryResponse))
  case requestResult of
    Right result -> pure result
    Left ex -> pure (Left (T.pack "Error while fetching definition"))
  where
    doRequest = runReq defaultHttpConfig $ do
      r <- req GET (https "api.dictionaryapi.dev" /: "api" /: "v2" /: "entries" /: "en" /: word) NoReqBody jsonResponse mempty
      let body = (responseBody r :: Value)
      let statusCode = responseStatusCode r

      case statusCode of
        200 -> do
          let decodedResponseBody = fromJSON body :: Result FreeDictionaryResponse
          case decodedResponseBody of
            Success result -> pure $ Right result
            Error err -> pure $ Left (T.pack err)
        _ -> pure $ Left (T.pack "Error while fetching definition")
