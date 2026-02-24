
module Controllers.Game.Model.UserEventSubscription (UserEvent(..), NotificationUpdate(..), newUserEventSubcriptionChannel, subscribeToUserEvents) where
    import Data.Text (Text)
    import ClassyPrelude (Bool, TChan, IO, STM)
    import Control.Concurrent.STM (dupTChan, newBroadcastTChanIO)
    import Controllers.Game.Model.ServerGame (ServerGame)
    import Repository.NotificationRepository (Notification)
    
    data NotificationUpdate = NotificationAdded Notification | NotificationsRead [Text]

    data UserEvent =
        MoveInUserGame { userEventGameId :: Text, userEventGameState :: ServerGame}
        |  GameOver { userEventGameId :: Text, userEventGameState :: ServerGame }
        |  NewGame { userEventGameId :: Text, userEventGameState :: ServerGame }
        |  PlayerActivityChanged { userEventGameId :: Text, activePlayerNames :: [Text] }
        | NotificationsChanged NotificationUpdate

    newUserEventSubcriptionChannel :: IO (TChan UserEvent)
    newUserEventSubcriptionChannel = newBroadcastTChanIO

    subscribeToUserEvents :: TChan UserEvent -> STM (TChan UserEvent)
    subscribeToUserEvents = dupTChan

