module Controllers.Game.Model.GameLobby (GameLobby(GameLobby), updatePlayers, game, lobbyPlayers, awaiting, channel) where

    import Prelude
    import Wordify.Rules.Game
    import Controllers.Game.Model.ServerPlayer
    import Controllers.GameLobby.Api
    import Control.Concurrent.STM.TChan
    import Controllers.GameLobby.Api

    -- TODO: Move this module to the GameLobby folder
    data GameLobby = GameLobby {game :: Game, lobbyPlayers :: [ServerPlayer], awaiting :: Int, channel :: TChan LobbyMessage}

    updatePlayers :: [ServerPlayer] -> GameLobby -> GameLobby
    updatePlayers players lobby = lobby {lobbyPlayers = players}