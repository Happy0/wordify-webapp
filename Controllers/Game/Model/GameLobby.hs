module Controllers.Game.Model.GameLobby (GameLobby(GameLobby)) where

    import Prelude
    import Controllers.Game.Model.ServerPlayer

    data GameLobby = GameLobby {players :: [ServerPlayer], awaiting :: Int}