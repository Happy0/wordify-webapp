module Controllers.Definition.FreeDictionaryService (getDefinitions) where

    import qualified Data.Text as T
    import Data.Aeson
    import Data.Aeson.Types
    import Controllers.Definition.DefinitionService
    import ClassyPrelude (IO, Either, undefined, Maybe, (<$>), pure, mapM, ($), toList) 
    
    data FreeDictionaryService = FreeDictionaryService {urlPrefix :: T.Text}

    data FreeDictionaryDefinition = FreeDictionaryDefinition {definition :: T.Text, synonyms :: [T.Text]}
    data FreeDictionaryMeaning = FreeDictionaryMeaning {partOfSpeech :: T.Text, definitions :: [FreeDictionaryDefinition]}
    data FreeDictionaryResponseItem = FreeDictionaryResponseItem {word :: T.Text, meanings :: [FreeDictionaryMeaning]}
    data FreeDictionaryResponse = FreeDictionaryResponse [FreeDictionaryResponseItem]

    instance FromJSON FreeDictionaryDefinition where
        parseJSON (Object response) = undefined

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

    instance DefinitionService FreeDictionaryService where
        getDefinitions service word = undefined
