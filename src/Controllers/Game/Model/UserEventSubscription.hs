
module Controllers.Game.Model.UserEventSubscription (UserEvent(..), newUserEventSubcriptionChannel, subscribeToUserEvents) where
    import Data.Text (Text)
    import Wordify.Rules.Game
    import ClassyPrelude (Bool, TChan, IO, (.), STM)
    import Control.Concurrent.STM (dupTChan, newBroadcastTChanIO)
    import Controllers.Game.Model.ServerGame (ServerGameSnapshot)
    
    data UserEvent = 
        MoveInUserGame { userEventGameId :: Text, userEventGameState :: ServerGameSnapshot, userEventUserToMove :: Bool }
        |  GameOver { userEventGameId :: Text, userEventGameState :: ServerGameSnapshot }
        |  NewGame { userEventGameId :: Text, userEventGameState :: ServerGameSnapshot, userEventUserToMove :: Bool }

    newUserEventSubcriptionChannel :: IO (TChan UserEvent)
    newUserEventSubcriptionChannel = newBroadcastTChanIO

    subscribeToUserEvents :: TChan UserEvent -> STM (TChan UserEvent)
    subscribeToUserEvents = dupTChan

