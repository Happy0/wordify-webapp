module Controllers.Game.Model.GameLobby (GameLobby(GameLobby)) where

    import Prelude
    import Wordify.Rules.Game
    import Controllers.Game.Model.ServerPlayer

    data GameLobby = GameLobby {game :: Game, players :: [ServerPlayer], awaiting :: Int}