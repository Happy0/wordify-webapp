module Controllers.Definition.FreeDictionaryService (getDefinitions) where

    import qualified Data.Text as T
    import Data.Aeson
    import Data.Aeson.Types
    import Controllers.Definition.DefinitionService
    import ClassyPrelude (IO, Either, undefined) 

    data FreeDictionaryService = FreeDictionaryService {urlPrefix :: T.Text}

    data FreeDictionaryDefinition = FreeDictionaryDefinition {definition :: T.Text, synonyms :: [T.Text]}
    data FreeDictionaryMeaning = FreeDictionaryMeaning {partOfSpeech :: T.Text, definitions :: [FreeDictionaryDefinition]}
    data FreeDictionaryResponseItem = FreeDictionaryResponseItem {meanings :: [FreeDictionaryMeaning]}
    data FreeDictionaryResponse = FreeDictionaryResponse [FreeDictionaryResponseItem]

    instance FromJSON FreeDictionaryDefinition where
        parseJSON (Object response) = undefined

    instance FromJSON FreeDictionaryMeaning where
        parseJSON (Object response) = undefined

    instance FromJSON FreeDictionaryResponseItem where
        parseJSON (Object response) = undefined

    instance FromJSON FreeDictionaryResponse where
        parseJSON (Object response) = undefined

    instance DefinitionService FreeDictionaryService where
        getDefinitions service word = undefined
