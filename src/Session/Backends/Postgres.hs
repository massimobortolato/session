{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Session.Backends.Postgres (
  DB,
  P.Query,
  initDb,
  newDbConnection,
)
where

import Control.Exception
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import Data.Pool
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple qualified as P
import Session.Session

--------------------------------------------------------------------------------
newtype DB = DB (Pool P.Connection)

--------------------------------------------------------------------------------
newDbConnection :: ByteString -> IO DB
newDbConnection connectionString =
  newPool config'
    >>= \pool -> pure $ DB pool
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

--------------------------------------------------------------------------------
initDb :: DB -> P.Query -> IO ()
initDb (DB pool) q =
  catch go handler
 where
  go = do
    withResource pool \conn ->
      P.execute_ conn q >> pure ()

  handler (SomeException e) = do
    putStrLn $ "DB error: " <> show e
    pure ()

--------------------------------------------------------------------------------
instance SessionBackend DB where
  newSession = newSession'
  getSession = getSession'
  newUser = newUser'
  getUser = getUser'

--------------------------------------------------------------------------------
newSession' :: DB -> Session -> SessionM Session
newSession' (DB pool) session =
  liftIO (catch iogo iohandler) >>= except
 where
  Session{token, csrf_token, last_request, ip_address, user = User{user_id}} = session

  iogo =
    withResource pool \conn ->
      P.execute
        conn
        "UPDATE users SET last_login=? WHERE id=?; \
        \INSERT INTO sessions (session_token, csrf_token, user_id, ip_address, last_request)\
        \VALUES (?, ?, ?, ?, ?)"
        (last_request, user_id, token, csrf_token, user_id, ip_address, last_request)
        >> pure (Right session)

  iohandler (SomeException e) =
    pure $ Left $ DatabaseError $ show e

--------------------------------------------------------------------------------
getSession' :: DB -> SessionToken -> IpAddress -> SessionM Session
getSession' (DB pool) token ipaddr =
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
        >>= pure . (extractSession)

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
newUser' (DB pool) user hash =
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
getUser' (DB pool) validemail hash =
  liftIO (catch iogo iohandler) >>= except
 where
  iogo = do
    withResource pool \conn ->
      P.query
        conn
        "SELECT id, fullname, last_login FROM users where email=? AND password_hash=?"
        (getValidatedEmail validemail, getPasswordHash hash)
        >>= pure . extractUser

  extractUser [] = Left NoUser
  extractUser [(id', fullname', lastLogin')] =
    Right $ User id' fullname' validemail lastLogin'
  extractUser _ = Left DuplicateUser

  iohandler (SomeException e) =
    pure $ Left $ DatabaseError $ show e
