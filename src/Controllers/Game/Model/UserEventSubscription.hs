
module Controllers.Game.Model.UserEventSubscription (UserEvent(..), newUserEventSubcriptionChannel, subscribeToUserEvents) where
    import Data.Text (Text)
    import Wordify.Rules.Game
    import ClassyPrelude (Bool, TChan, IO, (.), STM)
    import Control.Concurrent.STM (dupTChan, newBroadcastTChanIO)
    
    data UserEvent = MoveInUserGame { userEventGameId :: Text, userEventGameState :: Game, userEventUserToMove :: Bool }

    newUserEventSubcriptionChannel :: IO (TChan UserEvent)
    newUserEventSubcriptionChannel = newBroadcastTChanIO

    subscribeToUserEvents :: TChan UserEvent -> STM (TChan UserEvent)
    subscribeToUserEvents = dupTChan