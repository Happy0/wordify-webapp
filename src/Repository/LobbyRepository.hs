module Repository.LobbyRepository
  ( LobbyRepository (invitePlayer, getLobbyInvites),
    LobbyInvite (..),
  )
where

import ClassyPrelude (IO)
import qualified Data.Text as T

data LobbyInvite = LobbyInvite
  { inviteLobbyId :: T.Text,
    inviteFromUserId :: T.Text,
    inviteToUserId :: T.Text
  }

class LobbyRepository a where
  -- | Invite a player to a lobby by their username. Does nothing if the username does not exist.
  invitePlayer :: a -> T.Text -> T.Text -> T.Text -> IO ()
  -- | Get all invites for a given lobby.
  getLobbyInvites :: a -> T.Text -> IO [LobbyInvite]
