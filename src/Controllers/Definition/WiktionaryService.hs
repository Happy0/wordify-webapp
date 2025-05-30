{-# LANGUAGE InstanceSigs #-}
module Controllers.Definition.WiktionaryService (WiktionaryService, makeWiktionaryService) where
    import qualified Data.Text as T
    import Controllers.Definition.DefinitionService (DefinitionService (getDefinitions), getDefinitionsImpl, Definition(Definition))
    import ClassyPrelude (IO, undefined, Maybe, ($), pure, (<$>), traverse, SomeException)
    import Data.Either (Either (..))
    import qualified Data.Map as M
    import Data.Aeson
    import Data.Aeson.Types (withObject)
    import Data.Aeson ((.:))
    import ClassyPrelude (Either(Right))
    import Control.Exception (try)
    import Control.Monad ((>>=), mapM)
    import Network.HTTP.Req (runReq, defaultHttpConfig, GET (GET), req, https, (/:), NoReqBody (NoReqBody), jsonResponse, responseStatusCode, responseBody)
    import Data.Monoid (mempty)
    import Control.Error (note)
    import ClassyPrelude (sequence)
    import Data.List (concat)
    import Prelude (flip)
    import ClassyPrelude.Yesod (Maybe(..))
    import Text.HTML.TagSoup
    import qualified Data.List.Safe as L
    
    newtype WiktionaryService = WiktionaryService { languageShortcode :: T.Text }

    data WiktionaryDefinitionEntry = WiktionaryDefinitionEntry { definition :: T.Text, examples :: Maybe [T.Text] }

    data WiktionaryDefinition = WiktionaryDefinition {partOfSpeech :: T.Text, language :: T.Text, definitions :: [WiktionaryDefinitionEntry] }

    data WiktionaryDefinitionResponse = WiktionaryDefinitionResponse (M.Map T.Text [WiktionaryDefinition])

    instance FromJSON WiktionaryDefinitionEntry where
        parseJSON = withObject "DefinitionEntry" $ \obj -> do
            definition <- obj .: "definition"
            examples <- obj .:? "examples"
            pure (WiktionaryDefinitionEntry definition examples)

    instance FromJSON WiktionaryDefinition where
        parseJSON = withObject "Definition" $ \obj -> do
            definitions <- obj .: "definitions"
            partOfSpeech <- obj .: "partOfSpeech"
            language <- obj .: "language"
            pure (WiktionaryDefinition partOfSpeech language definitions)

    instance FromJSON WiktionaryDefinitionResponse where
        parseJSON obj = WiktionaryDefinitionResponse <$> parseJSON obj

    instance DefinitionService WiktionaryService where
        getDefinitions :: WiktionaryService -> T.Text -> IO (Either T.Text [Definition])
        getDefinitions = wiktionaryGetDefinitionsImpl

    stripHtml :: T.Text -> Either T.Text T.Text
    stripHtml word = do
        let tags = parseTags word
        pure (innerText tags)

    mapWithStrippedHtml :: WiktionaryDefinitionEntry -> T.Text -> Either T.Text Definition
    mapWithStrippedHtml entry partOfSpeech = do
        definition <- stripHtml (definition entry)
        let example =  examples entry >>= safeHead
        pure (Definition partOfSpeech definition example)
        where
            safeHead :: [a] -> Maybe a
            safeHead [] = Nothing
            safeHead (x:_) = Just x

    mapDefinition :: WiktionaryDefinition -> Either T.Text [Definition]
    mapDefinition (WiktionaryDefinition partOfSpeech _ definitions) = mapM (`mapWithStrippedHtml` partOfSpeech) definitions

    mapResults :: WiktionaryService -> WiktionaryDefinitionResponse -> Either T.Text [Definition]
    mapResults (WiktionaryService language) (WiktionaryDefinitionResponse responseMap) = do
        definitions <- note "No definitions for language found" (M.lookup language responseMap)
        results <- mapM mapDefinition definitions
        pure (concat results)

    -- TODO: follow any cases of form-of-definition to root definition

    wiktionaryGetDefinitionsImpl :: WiktionaryService ->  T.Text -> IO (Either T.Text [Definition])
    wiktionaryGetDefinitionsImpl wiktionaryService@(WiktionaryService language) word = do
        requestResult <- try doRequest :: IO (Either SomeException (Either T.Text WiktionaryDefinitionResponse))
        case requestResult of
            Right result -> pure $ result >>= mapResults wiktionaryService
            Left ex -> pure (Left (T.pack "Error while fetching definition"))
        where
            doRequest = runReq defaultHttpConfig $ do
                r <- req GET (https "en.wiktionary.org" /: "api" /: "rest_v1" /: "page" /: "definition" /: word) NoReqBody jsonResponse mempty
                let body = (responseBody r :: Value)
                let statusCode = responseStatusCode r

                case statusCode of
                    200 -> do
                        let decodedResponseBody = fromJSON body :: Result WiktionaryDefinitionResponse
                        case decodedResponseBody of
                            Success result -> pure $ Right result
                            Error err -> pure $ Left (T.pack err)
                    _ -> pure $ Left (T.pack "Error while fetching definition")

    makeWiktionaryService :: T.Text -> WiktionaryService
    makeWiktionaryService languageShortcode = WiktionaryService languageShortcode