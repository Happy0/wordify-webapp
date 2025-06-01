{-# LANGUAGE InstanceSigs #-}
module Controllers.Definition.WiktionaryService (WiktionaryService, makeWiktionaryService) where
    import qualified Data.Text as T
    import Controllers.Definition.DefinitionService (DefinitionService (getDefinitions), getDefinitionsImpl, Definition(Definition))
    import ClassyPrelude ( IO, undefined,
      Maybe,
      ($),
      pure,
      (<$>),
      traverse,
      SomeException,
      (.),
      not,
      (==), (++))
    import Data.Either (Either (..))
    import qualified Data.Map as M
    import Data.Aeson
    import Control.Exception (try)
    import Control.Monad ((>>=), mapM, (>>))
    import Network.HTTP.Req (runReq, defaultHttpConfig, GET (GET), req, https, (/:), NoReqBody (NoReqBody), jsonResponse, responseStatusCode, responseBody)
    import Data.Monoid (mempty)
    import Control.Error (note, ExceptT, runExceptT)
    import Data.List (concat)
    import ClassyPrelude.Yesod (Maybe(..), MonadIO (liftIO))
    import Text.HTML.TagSoup (Tag, parseTags, innerText, fromAttrib, (~==), isTagOpen, isTagText, isTagOpenName, sections)
    import qualified Data.List.Safe as L
    import ClassyPrelude (Bool)
    import ClassyPrelude (Bool(False))
    import ClassyPrelude (lift)
    import ClassyPrelude (rights)
    import Control.Monad.Error.Class (liftEither)
    import ClassyPrelude (head)
    import Text.HTML.TagSoup.Match (tagOpen)
    import ClassyPrelude (Int, join, const, mapMaybe, mapM_, print)
    import qualified Control.Arrow as A
    
    data WiktionaryService = WiktionaryService

    data WiktionaryDefinitionEntry = WiktionaryDefinitionEntry { definition :: T.Text, examples :: Maybe [T.Text] }
    data WiktionaryDefinition = WiktionaryDefinition {partOfSpeech :: T.Text, language :: T.Text, definitions :: [WiktionaryDefinitionEntry] }
    newtype WiktionaryDefinitionResponse = WiktionaryDefinitionResponse (M.Map T.Text [WiktionaryDefinition])

    data ProcessedWiktionaryResult = BaseDefinition Definition | LinkedDefinition { linkedWordInBaseForm :: T.Text, rawDefinition :: Definition }

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
        getDefinitions :: WiktionaryService -> T.Text -> T.Text -> IO (Either T.Text [Definition])
        getDefinitions = wiktionaryGetDefinitionsImpl

    stripHtml :: T.Text -> Either T.Text T.Text
    stripHtml word = do
        let tags = parseTags word
        pure (innerText tags)

    mapWithStrippedHtml :: WiktionaryDefinitionEntry -> T.Text -> Either T.Text ProcessedWiktionaryResult
    mapWithStrippedHtml entry partOfSpeech = do
        definitionWithHtmlStripped <- stripHtml (definition entry)
        let example =  examples entry >>= safeHead
        let definition = Definition partOfSpeech definitionWithHtmlStripped example
        let baseWord = extractBaseWord entry
        case baseWord of
            Nothing -> Right (BaseDefinition definition)
            Just word -> Right (LinkedDefinition word definition)
        where
            safeHead :: [a] -> Maybe a
            safeHead [] = Nothing
            safeHead (x:_) = Just x

    mapDefinition :: WiktionaryDefinition -> Either T.Text [ProcessedWiktionaryResult]
    mapDefinition (WiktionaryDefinition partOfSpeech _ definitions) = mapM (`mapWithStrippedHtml` partOfSpeech) definitions

    filterEmptyDefinitions :: [ProcessedWiktionaryResult] -> [ProcessedWiktionaryResult]
    filterEmptyDefinitions  = L.filter (not . isEmptyDefinition)
        where
            isEmptyDefinition :: ProcessedWiktionaryResult -> Bool
            isEmptyDefinition (BaseDefinition (Definition partOfSpeech definition _)) = T.null definition
            isEmptyDefinition (LinkedDefinition _ (Definition partOfSpeech definition _)) = T.null definition

    filterSymbolDefinitions :: [WiktionaryDefinition] -> [WiktionaryDefinition]
    filterSymbolDefinitions = L.filter (not . isSymbolPartOfSpeech)
        where
            isSymbolPartOfSpeech (WiktionaryDefinition partOfSpeech _ _) = partOfSpeech == "Symbol"

    mapDefinitions :: [WiktionaryDefinition] -> Either T.Text [ProcessedWiktionaryResult]
    mapDefinitions definitions = do
        results <- mapM mapDefinition (filterSymbolDefinitions definitions)
        pure (filterEmptyDefinitions (concat results))

    extractBaseWord :: WiktionaryDefinitionEntry -> Maybe T.Text
    extractBaseWord def@(WiktionaryDefinitionEntry definition _) = do
        if isFormOfDefinition def then do
            let htmlTags = parseTags definition
            let anchors = L.filter (isTagOpenName "a") htmlTags
            extractWord anchors
        else do
            Nothing

        where
            isFormOfDefinition :: WiktionaryDefinitionEntry -> Bool
            isFormOfDefinition (WiktionaryDefinitionEntry definition _) = "form-of-definition use-with-mention" `T.isInfixOf` definition

            extractWord :: [Tag T.Text] -> Maybe T.Text
            extractWord tag = L.last tag >>= getBaseWord

            getBaseWord :: Tag T.Text -> Maybe T.Text
            getBaseWord tag = do
                --(unsafePerformIO $ Just <$> print tag)
                let word = fromAttrib "title" tag
                case word of
                    "" -> Nothing
                    w -> Just w

    mapResults :: T.Text -> WiktionaryDefinitionResponse  -> Either T.Text [ProcessedWiktionaryResult]
    mapResults languageShortcode (WiktionaryDefinitionResponse responseMap) = do
        definitions <- note "No definitions for language found" (M.lookup languageShortcode responseMap)
        mapDefinitions definitions

    getDefinitionsAtCurrentDepth :: [ProcessedWiktionaryResult] -> [Definition]
    getDefinitionsAtCurrentDepth = L.map getDefinition
        where
            getDefinition :: ProcessedWiktionaryResult -> Definition
            getDefinition (BaseDefinition definition) = definition
            getDefinition (LinkedDefinition _ definition) = definition

    getLinkedWords :: [ProcessedWiktionaryResult] -> [T.Text]
    getLinkedWords = mapMaybe linkedWord
        where
            linkedWord :: ProcessedWiktionaryResult -> Maybe T.Text
            linkedWord (LinkedDefinition word _) = Just word
            linkedWord _ = Nothing

    filterOnlyBaseWords :: [ProcessedWiktionaryResult] -> [Definition]
    filterOnlyBaseWords = mapMaybe baseDefinition
        where
            baseDefinition :: ProcessedWiktionaryResult -> Maybe Definition
            baseDefinition (BaseDefinition def) = Just def
            baseDefinition _ = Nothing

    getDefinitionsWithLinkFollowingDepth :: T.Text -> T.Text -> Int -> ExceptT T.Text IO [Definition]
    getDefinitionsWithLinkFollowingDepth word localeShortCode 0 = do
        requestResult <- liftIO (try (doRequest word) :: IO (Either SomeException (Either T.Text WiktionaryDefinitionResponse)))
        requestResultWithErrorMapped <- liftEither $ join (A.left (const "Error while fetching definition") requestResult)
        mappedResults <- liftEither $ mapResults localeShortCode requestResultWithErrorMapped
        pure (getDefinitionsAtCurrentDepth mappedResults)

    getDefinitionsWithLinkFollowingDepth word localeShortCode _ = do
        requestResult <- liftIO (try (doRequest word) :: IO (Either SomeException (Either T.Text WiktionaryDefinitionResponse)))
        requestResultWithErrorMapped <- liftEither $ join (A.left (const "Error while fetching definition") requestResult :: Either T.Text (Either T.Text WiktionaryDefinitionResponse))
        mappedResults <- liftEither $ mapResults localeShortCode requestResultWithErrorMapped
        let linkedWords = getLinkedWords mappedResults

        linkedDefinitions <- concat <$> mapM (\linkedWord -> getDefinitionsWithLinkFollowingDepth linkedWord localeShortCode avoidAccidentallyDOSingWiktionaryDepth) linkedWords
        let otherDefinitions = filterOnlyBaseWords mappedResults
        pure (linkedDefinitions ++ otherDefinitions)
        where
            avoidAccidentallyDOSingWiktionaryDepth = 0

    wiktionaryGetDefinitionsImpl :: WiktionaryService ->  T.Text -> T.Text -> IO (Either T.Text [Definition])
    wiktionaryGetDefinitionsImpl _ word localeShortcode = runExceptT $ getDefinitionsWithLinkFollowingDepth word localeShortcode linkFollowingDepth
        where
            linkFollowingDepth = 1

    doRequest :: T.Text -> IO (Either T.Text WiktionaryDefinitionResponse)
    doRequest word = runReq defaultHttpConfig $ do
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

    makeWiktionaryService :: WiktionaryService
    makeWiktionaryService = WiktionaryService
