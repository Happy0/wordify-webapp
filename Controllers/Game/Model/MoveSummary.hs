module Controllers.Game.Model.MoveSummary(MoveSummary(Passed, Exchanged, Scored, Finished), toSummary) where

    import Prelude
    import Wordify.Rules.FormedWord
    import Wordify.Rules.Player
    import Wordify.Rules.Move

    data MoveSummary = Passed String | Exchanged String | Scored String FormedWords | Finished

    toSummary :: GameTransition -> MoveSummary
    toSummary (ExchangeTransition _ player _) = Exchanged $ name player
    toSummary (PassTransition _) = Passed "_"
    toSummary (GameFinished _ _ _) = Finished
    toSummary (MoveTransition player _ formed) = Scored (name player) formed