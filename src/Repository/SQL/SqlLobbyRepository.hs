module Repository.SQL.SqlLobbyRepository (SqlLobbyRepositoryBackend (SqlLobbyRepositoryBackend)) where

import ClassyPrelude (IO, Maybe (Just, Nothing), fmap, flip, return, ($), (.))
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist.Sql
import qualified Model as M
import Repository.LobbyRepository (LobbyRepository (invitePlayer, getLobbyInvites), LobbyInvite (..), InvitePlayerResult (..))

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

getLobbyInvitesSQL :: Pool SqlBackend -> T.Text -> IO [LobbyInvite]
getLobbyInvitesSQL pool gameLobbyId =
  withPool pool $ do
    inviteEntities <- selectList [M.LobbyInviteLobby ==. M.LobbyKey gameLobbyId] []
    return $ fmap inviteFromEntity inviteEntities
  where
    inviteFromEntity (Entity _ (M.LobbyInvite (M.LobbyKey lobbyId) (M.UserKey fromId) (M.UserKey toId))) =
      LobbyInvite lobbyId fromId toId

withPool = flip runSqlPersistMPool
