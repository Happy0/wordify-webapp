module Controllers.Game.Model.ServerGame (ServerGame(ServerGame)) where

    import Prelude
    import Data.Text
    import Wordify.Rules.Game
    import Model.Api

    data ServerGame = ServerGame GameID Game