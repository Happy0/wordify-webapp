module Controllers.Definition.RaeApiService (RaeApiService, makeRaeApiService) where

import ClassyPrelude (Bool (..), Either (Left, Right), IO, Maybe (..), Show, SomeException, concatMap, fmap, maybe, mempty, pure, try, ($))
import Controllers.Definition.DefinitionService (Definition (Definition), DefinitionService (getDefinitions))
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Req

-- | Service for looking up Spanish definitions from the Real Academia Española
-- (RAE) via the unofficial rae-api.com API.
data RaeApiService = RaeApiService {apiKey :: Maybe T.Text}

-- Internal response types

data RaeSense = RaeSense
  { senseCategory :: Maybe T.Text
  , senseDescription :: Maybe T.Text
  }
  deriving (Show)

newtype RaeMeaning = RaeMeaning
  { meaningSenses :: Maybe [RaeSense]
  }
  deriving (Show)

newtype RaeWordEntry = RaeWordEntry
  { entryMeanings :: Maybe [RaeMeaning]
  }
  deriving (Show)

data RaeWordEntryResponse = RaeWordEntryResponse
  { responseOk :: Bool
  , responseData :: Maybe RaeWordEntry
  }
  deriving (Show)

instance FromJSON RaeSense where
  parseJSON = withObject "Sense" $ \obj -> do
    category <- obj .:? "category"
    description <- obj .:? "description"
    pure $ RaeSense category description

instance FromJSON RaeMeaning where
  parseJSON = withObject "Meaning" $ \obj -> do
    senses <- obj .:? "senses"
    pure $ RaeMeaning senses

instance FromJSON RaeWordEntry where
  parseJSON = withObject "WordEntry" $ \obj -> do
    meanings <- obj .:? "meanings"
    pure $ RaeWordEntry meanings

instance FromJSON RaeWordEntryResponse where
  parseJSON = withObject "WordEntryResponse" $ \obj -> do
    ok <- obj .: "ok"
    dat <- obj .:? "data"
    pure $ RaeWordEntryResponse ok dat

instance DefinitionService RaeApiService where
  getDefinitions service word "es" = raeGetDefinitionsImpl service word
  getDefinitions _ _ _ = pure (Right [])

makeRaeApiService :: Maybe T.Text -> RaeApiService
makeRaeApiService = RaeApiService

raeGetDefinitionsImpl :: RaeApiService -> T.Text -> IO (Either T.Text [Definition])
raeGetDefinitionsImpl service word = do
  requestResult <- try (doRequest service word) :: IO (Either SomeException (Either T.Text RaeWordEntryResponse))
  case requestResult of
    Right result -> pure (fmap responseToDefinitions result)
    Left _ -> pure (Left "Error while fetching definition")

responseToDefinitions :: RaeWordEntryResponse -> [Definition]
responseToDefinitions (RaeWordEntryResponse _ Nothing) = []
responseToDefinitions (RaeWordEntryResponse _ (Just entry)) =
  case entryMeanings entry of
    Nothing -> []
    Just meanings -> concatMap meaningToDefinitions meanings

meaningToDefinitions :: RaeMeaning -> [Definition]
meaningToDefinitions (RaeMeaning Nothing) = []
meaningToDefinitions (RaeMeaning (Just senses)) = concatMap senseToDefinitions senses

senseToDefinitions :: RaeSense -> [Definition]
senseToDefinitions (RaeSense (Just category) (Just desc)) = [Definition category desc Nothing]
senseToDefinitions _ = []

doRequest :: RaeApiService -> T.Text -> IO (Either T.Text RaeWordEntryResponse)
doRequest service word = runReq noCheckConfig $ do
  let queryParams = maybe mempty ("api_key" =:) (apiKey service)
  r <- req GET (https "rae-api.com" /: "api" /: "words" /: word) NoReqBody jsonResponse queryParams
  let body = responseBody r :: Value
  let statusCode = responseStatusCode r
  case statusCode of
    200 -> case fromJSON body :: Result RaeWordEntryResponse of
      Success result -> pure $ Right result
      Error err -> pure $ Left (T.pack err)
    404 -> pure $ Right (RaeWordEntryResponse False Nothing)
    _ -> pure $ Left "Error while fetching definition"
  where
    -- Disable req's default non-2xx exception so we can handle 404
    -- (word not found) as an empty result rather than an error.
    noCheckConfig = defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing}
