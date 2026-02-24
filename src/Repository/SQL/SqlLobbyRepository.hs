module Repository.SQL.SqlLobbyRepository (SqlLobbyRepositoryBackend (SqlLobbyRepositoryBackend)) where

import ClassyPrelude (IO, Maybe (Just, Nothing), fmap, flip, return, ($), (.))
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist.Sql
import qualified Model as M
import Repository.LobbyRepository (LobbyRepository (invitePlayer, getLobbyInvites), InvitePlayerResult (..))

newtype SqlLobbyRepositoryBackend = SqlLobbyRepositoryBackend (Pool SqlBackend)

instance LobbyRepository SqlLobbyRepositoryBackend where
  invitePlayer (SqlLobbyRepositoryBackend pool) = invitePlayerSQL pool
  getLobbyInvites (SqlLobbyRepositoryBackend pool) = getLobbyInvitesSQL pool

invitePlayerSQL :: Pool SqlBackend -> T.Text -> T.Text -> T.Text -> IO InvitePlayerResult
invitePlayerSQL pool gameLobbyId invitedUsername inviterUserId =
  withPool pool $ do
    maybeUser <- selectFirst [M.UserUsername ==. Just invitedUsername] []
    case maybeUser of
      Nothing -> return InvitedUsernameNotFound
      Just (Entity (M.UserKey invitedUserId) _) -> do
        insert_ (M.LobbyInvite (M.LobbyKey gameLobbyId) (M.UserKey inviterUserId) (M.UserKey invitedUserId))
        return InvitePlayerSuccess

getLobbyInvitesSQL :: Pool SqlBackend -> T.Text -> IO [T.Text]
getLobbyInvitesSQL pool gameLobbyId =
  withPool pool $ do
    results <- rawSql
      "SELECT u.username FROM lobby_invite li JOIN \"user\" u ON u.ident = li.invite_to WHERE li.lobby = ? AND u.username IS NOT NULL"
      [PersistText gameLobbyId]
    return [uname | Single (Just uname) <- results]

withPool = flip runSqlPersistMPool
