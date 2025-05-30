
module Model.GameSetup (LocalisedGameSetup(GameSetup), localisedDictionary, localisedLetterBag, extraRules) where
    import Wordify.Rules.Dictionary (Dictionary)
    import Wordify.Rules.LetterBag (LetterBag)
    import Wordify.Rules.Extra.ExtraRule (ExtraRule)
    
    data LocalisedGameSetup = GameSetup { 
        localisedDictionary :: Dictionary,
        localisedLetterBag :: LetterBag,
        extraRules :: [ExtraRule]
    }
