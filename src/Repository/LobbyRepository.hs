module Repository.LobbyRepository
  ( LobbyRepository (invitePlayer, getLobbyInvites),
    LobbyInvite (..),
    InvitePlayerResult (..),
  )
where

import ClassyPrelude (IO)
import qualified Data.Text as T

data LobbyInvite = LobbyInvite
  { inviteLobbyId :: T.Text,
    inviteFromUserId :: T.Text,
    inviteToUserId :: T.Text
  }

data InvitePlayerResult = InvitePlayerSuccess | InvitedUsernameNotFound

class LobbyRepository a where
  -- | Invite a player to a lobby by their username. Returns InvitedUsernameNotFound if the username does not exist.
  invitePlayer :: a -> T.Text -> T.Text -> T.Text -> IO InvitePlayerResult
  -- | Get all invites for a given lobby.
  getLobbyInvites :: a -> T.Text -> IO [LobbyInvite]
