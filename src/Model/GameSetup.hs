
module Model.GameSetup (LocalisedGameSetup(GameSetup), localisedDictionary, localisedLetterBag, extraRules, gameLanguageShortCode, tileLettersToValueMap, TileValues) where
    import Wordify.Rules.Dictionary (Dictionary)
    import Wordify.Rules.LetterBag (LetterBag)
    import Wordify.Rules.Extra.ExtraRule (ExtraRule)
    import qualified Data.Text as T
    import Data.Map
    import ClassyPrelude (Int)
    
    type TileValues = Map T.Text Int
    
    data LocalisedGameSetup = GameSetup {
        gameLanguageShortCode :: T.Text,
        localisedDictionary :: Dictionary,
        localisedLetterBag :: LetterBag,
        extraRules :: [ExtraRule],
        tileLettersToValueMap :: TileValues
    }
