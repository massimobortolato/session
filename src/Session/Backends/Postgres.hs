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
import Session.Session

--------------------------------------------------------------------------------
data DB = DB
  { pool :: Pool P.Connection
  , usersTable :: String
  , sessionsTable :: String
  }

--------------------------------------------------------------------------------
newDbConnection :: ByteString -> String -> String -> IO DB
newDbConnection connectionString usersTableName sessionsTableName =
  newPool config'
    >>= \pool ->
      withResource pool (`P.execute_` initQuery)
        >> pure
          DB
            { pool = pool
            , usersTable = usersTableName
            , sessionsTable = sessionsTableName
            }
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
    CREATE TABLE users IF NOT EXISTS (
      id               UUID NOT NULL UNIQUE PRIMARY KEY,
      fullname         TEXT NOT NULL,
      email            TEXT UNIQUE NOT NULL,
      password_hash    TEXT NOT NULL,
      created_at       TIMESTAMP WITH TIME ZONE NOT NULL,
      last_login       TIMESTAMP WITH TIME ZONE
    );
    CREATE TABLE sessions IF NOT EXISTS (
      session_token    TEXT NOT NULL UNIQUE PRIMARY KEY,
      csrf_token       TEXT NOT NULL UNIQUE,
      user_id          UUID NOT NULL,
      ip_address       TEXT NOT NULL,
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
newSession' DB{pool, usersTable, sessionsTable} session =
  liftIO (catch iogo iohandler) >>= except
 where
  Session{token, csrf_token, last_request, ip_address, user = User{user_id}} = session

  iogo =
    withResource pool \conn ->
      P.execute
        conn
        "UPDATE ? SET last_login=? WHERE id=?; \
        \INSERT INTO ? (session_token, csrf_token, user_id, ip_address, last_request)\
        \VALUES (?, ?, ?, ?, ?)"
        (usersTable, last_request, user_id, sessionsTable, token, csrf_token, user_id, ip_address, last_request)
        >> pure (Right session)

  iohandler (SomeException e) =
    pure $ Left $ DatabaseError $ show e

--------------------------------------------------------------------------------
getSession' :: DB -> SessionToken -> IpAddress -> SessionM Session
getSession' DB{pool} token ipaddr =
  liftIO (catch iogo iohandler) >>= except
 where
  iogo = do
    withResource pool \conn ->
      P.query
        conn
        "SELECT s.csrf_token, s.last_request, u.id, u.fullname, u.email, u.last_login \
        \FROM sessions s INNER JOIN users u ON s.user_id = u.id \
        \WHERE session_token = ? AND ip_address = ? \
        \"
        (token, ipaddr)
        <&> extractSession

  extractSession [] = Left NoSession
  extractSession ((csrf', ts', uid', fullname', email', lastLogin') : _)
    | Just validatedEmail' <- validateEmail email' =
        let
          user = User uid' fullname' validatedEmail' lastLogin'
          session = Session token csrf' ts' ipaddr user
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
        "INSERT INTO users (id, fullname, email, password_hash, created_at) VALUES (?, ?, ?, ?, ?)"
        (user_id, fullname, getValidatedEmail email, getPasswordHash hash, now)
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
        "SELECT id, fullname, last_login FROM users where email=? AND password_hash=?"
        (getValidatedEmail validemail, getPasswordHash hash)
        <&> extractUser

  extractUser [] = Left NoUser
  extractUser [(id', fullname', lastLogin')] =
    Right $ User id' fullname' validemail lastLogin'
  extractUser _ = Left DuplicateUser

  iohandler (SomeException e) =
    pure $ Left $ DatabaseError $ show e
