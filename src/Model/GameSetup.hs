
module Model.GameSetup (LocalisedGameSetup(GameSetup), localisedDictionary, localisedLetterBag, extraRules, gameLanguageShortCode) where
    import Wordify.Rules.Dictionary (Dictionary)
    import Wordify.Rules.LetterBag (LetterBag)
    import Wordify.Rules.Extra.ExtraRule (ExtraRule)
    import qualified Data.Text as T
    
    data LocalisedGameSetup = GameSetup {
        gameLanguageShortCode :: T.Text,
        localisedDictionary :: Dictionary,
        localisedLetterBag :: LetterBag,
        extraRules :: [ExtraRule]
    }
