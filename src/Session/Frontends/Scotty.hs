{-# LANGUAGE OverloadedStrings #-}

module Session.Frontends.Scotty (
  ScottyAppSettings (..),
  ScottyAppM,
  ActionM,
  scottyS,
  withLogin,
  withSignup,
  withSession,
  module Web.Scotty.Trans,
)
where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.Encoding qualified as T (encodeUtf8)
import Network.Wai (Request (remoteHost))
import Session.Session
import Web.Scotty.Cookie qualified as CK
import Web.Scotty.Trans

--------------------------------------------------------------------------------
data ScottyAppSettings backend = ScottyAppSettings
  { db :: backend
  , sessionConfig :: SessionConfig
  , sessionCookieName :: Text
  }

newtype ScottyAppM backend a = ScottyAppM
  { runScottyAppM :: ReaderT (ScottyAppSettings backend) IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (ScottyAppSettings backend))
type ActionM backend = ActionT (ScottyAppM backend)

--------------------------------------------------------------------------------
scottyS :: (SessionBackend d) => Int -> ScottyAppSettings d -> ScottyT (ScottyAppM d) () -> IO ()
scottyS port config = scottyT port runInIO
 where
  runInIO =
    flip (runReaderT . runScottyAppM) config

--------------------------------------------------------------------------------
withLogin ::
  (SessionBackend d) =>
  (SessionError -> ActionM d ()) ->
  (Session -> ActionM d ()) ->
  ActionM d ()
withLogin handler job = do
  db <- asks db
  cfg <- asks sessionConfig
  cookieName <- asks sessionCookieName
  email <- formParam "email"
  password <- formParam "password"
  ip <- request <&> remoteHost
  withSessionM
    (login db cfg email password ip)
    handler
    (\session -> setSessionCookie cfg cookieName session >> job session)

--------------------------------------------------------------------------------
withSignup ::
  (SessionBackend d) =>
  (SessionError -> ActionM d ()) ->
  (Session -> ActionM d ()) ->
  ActionM d ()
withSignup handler job = do
  db <- asks db
  cfg <- asks sessionConfig
  cookieName <- asks sessionCookieName
  fullname <- formParam "fullname"
  email <- formParam "email"
  password <- formParam "password"
  confirmpassword <- formParam "confirmpassword"
  ip <- request <&> remoteHost
  withSessionM
    (signup' db cfg fullname email password confirmpassword ip)
    handler
    (\session -> setSessionCookie cfg cookieName session >> job session)

--------------------------------------------------------------------------------
withSession ::
  (SessionBackend d) =>
  (SessionError -> ActionM d ()) ->
  (Session -> ActionM d ()) ->
  ActionM d ()
withSession handler job = do
  db <- asks db
  cfg <- asks sessionConfig
  cookieName <- asks sessionCookieName
  cookie <- CK.getCookie cookieName
  ip <- request <&> remoteHost
  withSessionM
    (getSessionCookie cookie >>= \token -> isSession db cfg token ip)
    handler
    job

--------------------------------------------------------------------------------
--    PRIVATE FUNCTIONS
--------------------------------------------------------------------------------
setSessionCookie ::
  (SessionBackend d) =>
  SessionConfig ->
  Text ->
  Session ->
  ActionM d ()
setSessionCookie SessionConfig{sessionExpireSeconds} cookieName Session{token} =
  CK.setCookie $
    CK.defaultSetCookie
      { CK.setCookieName = T.encodeUtf8 cookieName
      , CK.setCookieValue = token
      , CK.setCookieHttpOnly = True
      , CK.setCookieSecure = False
      , CK.setCookieSameSite = Just CK.sameSiteStrict
      , CK.setCookieMaxAge = Just $ fromIntegral sessionExpireSeconds
      }

-- --------------------------------------------------------------------------------
getSessionCookie :: Maybe Text -> SessionM ByteString
getSessionCookie Nothing = toSessionKo NoSession
getSessionCookie (Just cookie) = toSessionOk $ T.encodeUtf8 cookie
