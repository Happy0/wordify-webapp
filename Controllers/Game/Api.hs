module Controllers.Game.Api (GameCreated(GameCreated)) where

    import Model.Api
    import Model.ServerGame
    import Data.Aeson
    import Data.Text

    data GameCreated = GameCreated GameId

    instance ToJSON GameCreated where
        toJSON (GameCreated gameId) = object ["gameId" .= gameId]

    instance ServerMessage GameCreated where
        commandName _ = pack "gameCreated"