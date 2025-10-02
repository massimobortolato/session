{-# LANGUAGE OverloadedStrings #-}

module Session.Session (
  withSessionM,
  isSession,
  login,
  signup,
  signup',
  Session (..),
  User (..),
  SessionError (..),
  SessionBackend (..),
  SessionM,
  SessionToken,
  SessionCsfrToken,
  Email,
  ValidatedEmail,
  Password,
  PasswordHash,
  HashSalt,
  IpAddress,
  SessionConfig (..),
  validateEmail,
  getValidatedEmail,
  getPasswordHash,
  toSessionOk,
  toSessionKo,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Crypto.Error (CryptoFailable (CryptoPassed))
import Crypto.KDF.Argon2 (Options (iterations))
import Crypto.KDF.Argon2 qualified as Argon2
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Base64.Types qualified as B64
import Data.ByteString as BS (ByteString, take, takeEnd)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 as B8 (pack, takeWhile)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Network.Socket (SockAddr)
import Text.Regex.TDFA ((=~))

--------------------------------------------------------------------------------
type SessionToken = ByteString
type SessionCsfrToken = ByteString
type Email = ByteString
type Password = ByteString
type HashSalt = ByteString
type IpAddress = ByteString

--------------------------------------------------------------------------------
newtype ValidatedEmail = ValidatedEmail Email deriving newtype (Show)
newtype PasswordHash = PasswordHash ByteString

--------------------------------------------------------------------------------
data SessionConfig = SessionConfig
  { hashSalt :: HashSalt
  , sessionExpireSeconds :: Int
  }

--------------------------------------------------------------------------------
data Session = Session
  { token :: SessionToken
  , csrf_token :: SessionCsfrToken
  , last_request :: UTCTime
  , ip_address :: IpAddress
  , user :: User
  }

--------------------------------------------------------------------------------
data User = User
  { user_id :: UUID
  , fullname :: Text
  , email :: ValidatedEmail
  , last_login :: Maybe UTCTime
  }

--------------------------------------------------------------------------------
data SessionError
  = InvalidFullname
  | InvalidEmailAddress
  | InvalidPassword
  | PasswordsMismatch
  | PasswordHashFail
  | NoUser
  | NoSession
  | DuplicateUser
  | DatabaseError String

--------------------------------------------------------------------------------
type SessionM = ExceptT SessionError IO

--------------------------------------------------------------------------------
class SessionBackend b where
  newSession :: b -> Session -> SessionM Session
  getSession :: b -> SessionToken -> IpAddress -> SessionM Session
  newUser :: b -> User -> PasswordHash -> SessionM User
  getUser :: b -> ValidatedEmail -> PasswordHash -> SessionM User

--------------------------------------------------------------------------------
isSession ::
  (SessionBackend b) =>
  b ->
  SessionConfig ->
  SessionToken ->
  SockAddr ->
  SessionM Session
isSession db cfg token ip = do
  session <- getSession db token (sockToIpAddress ip)
  now <- liftIO getCurrentTime
  case diffUTCTime now (last_request session) > expirationInterval of
    True -> toSessionKo NoSession
    False -> pure session
 where
  expirationInterval = fromIntegral $ sessionExpireSeconds cfg

--------------------------------------------------------------------------------
login :: (SessionBackend b) => b -> SessionConfig -> Email -> Password -> SockAddr -> SessionM Session
login db cfg email password ip = do
  validEmail <- validateEmail' email
  passwdHash <- hashPassword (hashSalt cfg) password
  user <- getUser db validEmail passwdHash
  session <- mkNewSession user $ sockToIpAddress ip
  session' <- newSession db session
  toSessionOk session'

--------------------------------------------------------------------------------
signup :: (SessionBackend b) => b -> SessionConfig -> Text -> Email -> Password -> SockAddr -> SessionM Session
signup db cfg fullname email password ip = do
  validEmail <- validateEmail' email
  passwdHash <- hashPassword (hashSalt cfg) password
  uid <- liftIO nextRandom
  user <-
    newUser
      db
      User{user_id = uid, fullname = fullname, email = validEmail, last_login = Nothing}
      passwdHash
  session <- mkNewSession user $ sockToIpAddress ip
  session' <- newSession db session
  toSessionOk session'

--------------------------------------------------------------------------------
signup' :: (SessionBackend b) => b -> SessionConfig -> Text -> Email -> Password -> Password -> SockAddr -> SessionM Session
signup' db cfg fullname email password confirmpassword ip =
  if password == confirmpassword
    then signup db cfg fullname email password ip
    else toSessionKo PasswordsMismatch

--------------------------------------------------------------------------------
withSessionM ::
  (MonadIO m) =>
  SessionM a ->
  (SessionError -> m r) ->
  (a -> m r) ->
  m r
withSessionM sm handler f =
  liftIO (runExceptT sm)
    >>= \case
      Left err -> handler err
      Right res -> f res

--------------------------------------------------------------------------------
getValidatedEmail :: ValidatedEmail -> Email
getValidatedEmail (ValidatedEmail email) = email

--------------------------------------------------------------------------------
getPasswordHash :: PasswordHash -> ByteString
getPasswordHash (PasswordHash hash) = hash

--------------------------------------------------------------------------------
validateEmail :: Email -> Maybe ValidatedEmail
validateEmail email =
  case email =~ regex of
    True -> Just $ ValidatedEmail email
    False -> Nothing
 where
  regex = "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$" :: ByteString

--------------------------------------------------------------------------------
--      PRIVATE FUNCTIONS
--------------------------------------------------------------------------------
sockToIpAddress :: SockAddr -> IpAddress
sockToIpAddress = B8.takeWhile (/= ':') . B8.pack . show

--------------------------------------------------------------------------------
validateEmail' :: Email -> SessionM ValidatedEmail
validateEmail' email =
  case validateEmail email of
    Just validated -> toSessionOk validated
    Nothing -> toSessionKo InvalidEmailAddress

--------------------------------------------------------------------------------
hashPassword :: HashSalt -> Password -> SessionM PasswordHash
hashPassword salt pwd =
  case Argon2.hash opts pwd salt sz of
    CryptoPassed hash -> toSessionOk $ PasswordHash $ B64.extractBase64 $ B64.encodeBase64' hash
    _ -> toSessionKo PasswordHashFail
 where
  opts =
    Argon2.Options
      { iterations = 5
      , memory = 7168
      , parallelism = 1
      , variant = Argon2.Argon2id
      , version = Argon2.Version13
      }
  sz = 64

--------------------------------------------------------------------------------
mkNewSession :: User -> IpAddress -> SessionM Session
mkNewSession user ip = do
  (token, csrf_token) <- newSessionTokens
  time <- liftIO getCurrentTime
  toSessionOk $
    Session
      { token = token
      , csrf_token = csrf_token
      , last_request = time
      , ip_address = ip
      , user = user
      }

--------------------------------------------------------------------------------
toSessionOk :: a -> SessionM a
toSessionOk = except . Right

--------------------------------------------------------------------------------
toSessionKo :: SessionError -> SessionM a
toSessionKo = except . Left

--------------------------------------------------------------------------------
newSessionTokens :: SessionM (ByteString, ByteString)
newSessionTokens = do
  bytes <- liftIO $ getRandomBytes (session_id_sz + csrf_token_sz)
  let
    session_id = (B64.extractBase64 . B64.encodeBase64') (BS.take session_id_sz bytes)
    csrf_token = (B64.extractBase64 . B64.encodeBase64') (BS.takeEnd session_id_sz bytes)
  pure (session_id, csrf_token)
 where
  session_id_sz = 64
  csrf_token_sz = 64
