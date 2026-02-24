module Repository.LobbyRepository
  ( LobbyRepository (invitePlayer, getLobbyInvites),
    InvitePlayerResult (..),
  )
where

import ClassyPrelude (IO)
import qualified Data.Text as T

data InvitePlayerResult = InvitePlayerSuccess T.Text | InvitedUsernameNotFound | InvitedSelf

class LobbyRepository a where
  -- | Invite a player to a lobby by their username. Returns InvitedUsernameNotFound if the username does not exist.
  invitePlayer :: a -> T.Text -> T.Text -> T.Text -> IO InvitePlayerResult
  -- | Get the usernames of all players who have been invited to a given lobby but haven't yet joined.
  getLobbyInvites :: a -> T.Text -> IO [T.Text]
