{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Session.Backends.Postgres (
  DB,
  newDbConnection,
)
where

import Control.Exception
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Pool
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple qualified as P
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Session.Session

--------------------------------------------------------------------------------
newtype DB = DB {pool :: Pool P.Connection}

--------------------------------------------------------------------------------
newtype UserRoleDB = UserRoleDB UserRole
instance ToField UserRoleDB where
  toField (UserRoleDB RoleUser) = toField (0 :: Int)
  toField (UserRoleDB RoleAdmin) = toField (1 :: Int)
instance FromField UserRoleDB where
  fromField f mdata = do
    i :: Int <- fromField f mdata
    case i of
      0 -> pure $ UserRoleDB RoleUser
      _ -> pure $ UserRoleDB RoleAdmin

--------------------------------------------------------------------------------
newDbConnection :: ByteString -> IO DB
newDbConnection connectionString =
  newPool config'
    >>= \pool ->
      withResource pool (`P.execute_` initQuery)
        >> pure DB{pool = pool}
 where
  connect = P.connectPostgreSQL connectionString
  close = P.close
  idleTime = 30
  maxResources = 30
  config' = setNumStripes Nothing config
  config =
    defaultPoolConfig
      connect
      close
      idleTime
      maxResources
  initQuery =
    """
    CREATE TABLE IF NOT EXISTS users (
      id               UUID NOT NULL UNIQUE PRIMARY KEY,
      fullname         TEXT NOT NULL,
      email            TEXT UNIQUE NOT NULL,
      password_hash    TEXT NOT NULL,
      created_at       TIMESTAMP WITH TIME ZONE NOT NULL,
      last_login       TIMESTAMP WITH TIME ZONE,
      role             INT NOT NULL
    );
    CREATE TABLE IF NOT EXISTS sessions (
      session_token    TEXT NOT NULL UNIQUE PRIMARY KEY,
      csrf_token       TEXT NOT NULL UNIQUE,
      user_id          UUID NOT NULL,
      ip_address       TEXT NOT NULL,
      created_at       TIMESTAMP WITH TIME ZONE NOT NULL,
      last_request     TIMESTAMP WITH TIME ZONE NOT NULL
    );
    """

--------------------------------------------------------------------------------
instance SessionBackend DB where
  newSession = newSession'
  getSession = getSession'
  newUser = newUser'
  getUser = getUser'

--------------------------------------------------------------------------------
newSession' :: DB -> Session -> SessionM Session
newSession' DB{pool} session =
  liftIO (catch iogo iohandler) >>= except
 where
  Session{token, csrf_token, last_request, ip_address, created_at, user = User{user_id}} = session

  iogo =
    withResource pool \conn ->
      P.execute
        conn
        "UPDATE users SET last_login=? WHERE id=?; INSERT INTO sessions (session_token, csrf_token, user_id, ip_address, last_request, created_at) VALUES(?, ?, ?, ?, ?, ?)"
        (last_request, user_id, token, csrf_token, user_id, ip_address, last_request, created_at)
        >> pure (Right session)

  iohandler (SomeException e) =
    pure $ Left $ DatabaseError $ show e

--------------------------------------------------------------------------------
getSession' :: DB -> SessionToken -> IpAddress -> SessionM Session
getSession' DB{pool} token ipaddr =
  liftIO (catch iogo iohandler) >>= except
 where
  iogo = do
    now <- getCurrentTime
    withResource pool \conn ->
      P.query
        conn
        """
        UPDATE sessions s
        SET last_request = ?
        FROM users u
        WHERE s.session_token = ? 
          AND s.ip_address = ? 
          AND s.user_id = u.id
        RETURNING 
          s.csrf_token, OLD.last_request, s.created_at, u.id, u.fullname, u.email, u.last_login, u.role
        """
        (now, token, ipaddr)
        <&> extractSession

  extractSession [] = Left NoSession
  extractSession ((csrf', ts', cr', uid', fullname', email', lastLogin', UserRoleDB role') : _)
    | Just validatedEmail' <- validateEmail email' =
        let
          user = User{user_id = uid', fullname = fullname', email = validatedEmail', last_login = lastLogin', role = role'}
          session = Session{token = token, csrf_token = csrf', last_request = ts', created_at = cr', ip_address = ipaddr, user = user}
         in
          Right session
    | otherwise =
        Left $ DatabaseError "invalid email address"

  iohandler (SomeException e) =
    pure $ Left $ DatabaseError $ show e

--------------------------------------------------------------------------------
newUser' :: DB -> User -> PasswordHash -> SessionM User
newUser' DB{pool} user hash =
  liftIO (catch iogo iohandler) >>= except
 where
  User{user_id, fullname, email} = user

  iogo = do
    now <- getCurrentTime
    withResource pool \conn ->
      P.execute
        conn
        "INSERT INTO users (id, fullname, email, password_hash, created_at, role) VALUES (?, ?, ?, ?, ?, ?)"
        (user_id, fullname, getValidatedEmail email, getPasswordHash hash, now, UserRoleDB RoleUser)
        >> pure (Right user)

  iohandler (SomeException e) =
    pure $ Left $ DatabaseError $ show e

--------------------------------------------------------------------------------
getUser' :: DB -> ValidatedEmail -> PasswordHash -> SessionM User
getUser' DB{pool} validemail hash =
  liftIO (catch iogo iohandler) >>= except
 where
  iogo = do
    withResource pool \conn ->
      P.query
        conn
        "SELECT id, fullname, last_login, role FROM users where email=? AND password_hash=?"
        (getValidatedEmail validemail, getPasswordHash hash)
        <&> extractUser

  extractUser [] = Left NoUser
  extractUser [(id', fullname', lastLogin', UserRoleDB role')] =
    Right $ User{user_id = id', fullname = fullname', email = validemail, last_login = lastLogin', role = role'}
  extractUser _ = Left DuplicateUser

  iohandler (SomeException e) =
    pure $ Left $ DatabaseError $ show e
