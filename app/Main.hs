{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent qualified
import Data.Text.Lazy qualified as TL
import Pages
import Session.Backends.Postgres
import Session.Frontends.Scotty
import Session.Session
import Types (ActionS)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  db <- initDbConnection
  let
    config =
      SessionConfig
        { hashSalt = "1234567890qwertyuiop"
        , sessionAliveTime = Minutes 15
        , sessionExpireTime = Days 10
        }
    settings =
      ScottyAppSettings
        { db = db
        , sessionConfig = config
        , sessionCookieName = "id"
        }

  scottyS 8000 settings $ do
    get "/login" loginPage
    get "/signup" signupPage

    get "/" $ do
      let
        noSession _ = redirect "/login"
        yesSession = rootPageWithSession
      withSession noSession yesSession

    post "/login" $ do
      let
        ko _ = sleep 2 >> redirect "/login"
        ok _ = redirect "/"
      withLogin ko ok

    post "/signup" $ do
      let
        ko InvalidFullname = redirect "/signup?message=Invalid fullname"
        ko InvalidEmailAddress = redirect "/signup?message=Invalid email address"
        ko InvalidPassword = redirect "/signup?message=Invalid password"
        ko PasswordsMismatch = redirect "/signup?message=Passwords do not match"
        ko e = redirect $ "/signup?message=" <> TL.show e
        ok _ = redirect "/"
      withSignup ko ok
 where
  initDbConnection =
    newDbConnection "postgresql://postgres:pippo@localhost/sessions"

--------------------------------------------------------------------------------
sleep :: Int -> ActionS ()
sleep secs =
  liftIO $ Control.Concurrent.threadDelay $ secs * 1000000