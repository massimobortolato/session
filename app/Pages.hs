{-# LANGUAGE OverloadedStrings #-}

module Pages where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Text.Lazy qualified as TL
import Lucid
import Session.Session
import Types (ActionS)
import Web.Scotty.Trans

--------------------------------------------------------------------------------
type PageTitle = TL.Text
type ExtraHeadContent = Html ()
type BodyContent = Html ()

--------------------------------------------------------------------------------
rootPage :: ActionS ()
rootPage =
  html $
    renderText $
      bootstrapHead "Home" (pure ()) $ do
        div_ [class_ "container mt-3"] $ do
          nav_ [class_ "navbar"] $ do
            div_ [class_ "container justify-content-start"] $ do
              a_ [class_ "nav-link p-2", href_ "/"] "Home"
              a_ [class_ "nav-link p-2", href_ "/login"] "Login"
              a_ [class_ "nav-link p-2", href_ "/signup"] "Signup"

--------------------------------------------------------------------------------
rootPageWithSession :: Session -> ActionS ()
rootPageWithSession session =
  html $
    renderText $
      bootstrapHead "Home" (pure ()) $ do
        div_ [class_ "container mt-3"] $ do
          nav_ [class_ "navbar"] $ do
            div_ [class_ "container justify-content-start"] $ do
              a_ [class_ "nav-link p-2", href_ "/"] "Home"
              a_ [class_ "nav-link p-2", href_ "/login"] "Login"
              a_ [class_ "nav-link p-2", href_ "/signup"] "Signup"
        div_ $ do
          welcomeRow session
 where
  welcomeRow Session{user = User{fullname = fullname}} =
    h5_ $ toHtml $ "Buongiorno " <> fullname

--   do
--     db <- asks db
--     users <- liftIO $ getUsers db
--     maybeSession <- getSession
--     html $ pageHtml users maybeSession
--  where
--   pageHtml users maybeSession =
--     renderText $
--       bootstrapHead "Home" (pure ()) $ do
--         div_ [class_ "container mt-3"] $ do
--           nav_ [class_ "navbar"] $ do
--             div_ [class_ "container justify-content-start"] $ do
--               a_ [class_ "nav-link p-2", href_ "/"] "Home"
--               a_ [class_ "nav-link p-2", href_ "/login"] "Login"
--               a_ [class_ "nav-link p-2", href_ "/signup"] "Signup"
--           div_ $ do
--             welcomeRow maybeSession
--           div_ [class_ "card shadow"] $ do
--             div_ [class_ "card-body"] $ do
--               h3_ [class_ "card-title text-center mb-4"] "Database"
--               table_ [class_ "table"] $ do
--                 thead_ $ do
--                   th_ [scope_ "col"] "id"
--                   th_ [scope_ "col"] "fullname"
--                   th_ [scope_ "col"] "email"
--                   th_ [scope_ "col"] "last login"
--                 tbody_ $ do
--                   mconcat $ usersToRows users
--               form_ [action_ "/resetDb", method_ "get"] $ do
--                 div_ [class_ "d-grid"] $ do
--                   button_ [type_ "submit", class_ "btn btn-primary"] "Reset DB"

--   welcomeRow Nothing = pure ()
--   welcomeRow (Just Session{user = User{fullname = fullname}}) =
--     h5_ $ toHtml $ "Buongiorno " <> fullname

--   usersToRows =
--     map
--       ( \User{user_id, fullname, email, last_login} ->
--           tr_
--             ( td_ (toHtml $ show user_id)
--                 <> td_ (toHtml fullname)
--                 <> td_ (toHtml email)
--                 <> td_ (toHtml $ showM last_login)
--             )
--       )

--   showM (Just a) = show a
--   showM _ = ""

--------------------------------------------------------------------------------
loginPage :: ActionS ()
loginPage = do
  maybeWarning <- queryParamMaybe "message"
  html $ pageHtml maybeWarning
 where
  pageHtml :: Maybe TL.Text -> TL.Text
  pageHtml maybeWarning =
    renderText $
      bootstrapHead "Login" (pure ()) $ do
        div_ [class_ "container d-flex justify-content-center align-items-center vh-100"] $ do
          div_ [class_ "card shadow", style_ "width: 22rem;"] $ do
            div_ [class_ "card-body"] $ do
              h3_ [class_ "card-title text-center mb-4"] "Login"
              form_ [action_ "/login", method_ "post"] $ do
                div_ [class_ "mb-3"] $ do
                  label_ [for_ "email", class_ "form-label"] "Email address"
                  input_ [type_ "email", class_ "form-control", id_ "email", placeholder_ "Enter your email", required_ "required", name_ "email"]
                div_ [class_ "mb-3"] $ do
                  label_ [for_ "password", class_ "form-label"] "Password"
                  input_ [type_ "password", class_ "form-control", id_ "password", placeholder_ "Enter your password", required_ "required", name_ "password"]
                when (isJust maybeWarning) $ div_ [class_ "text-danger mb-3"] (toHtml (fromMaybe "" maybeWarning))
                div_ [class_ "d-grid"] $ do
                  button_ [type_ "submit", class_ "btn btn-primary"] "Login"
              p_ [class_ "text-center mt-3"] $ do
                a_ [href_ "/forgotpwd"] "Forgot password?"
              p_ [class_ "text-center"] $ do
                "Don't have an account? "
                a_ [href_ "/signup"] "Sign up"

--------------------------------------------------------------------------------
signupPage :: ActionS ()
signupPage = do
  maybeWarning <- queryParamMaybe "message"
  html $ pageHtml maybeWarning
 where
  pageHtml :: Maybe TL.Text -> TL.Text
  pageHtml maybeWarning =
    renderText $
      let
        extraHead =
          link_ [rel_ "stylesheet", href_ "css/style-signup.css"]
        bodyContent =
          div_ [class_ "container d-flex justify-content-center align-items-center vh-100"] $ do
            div_ [class_ "card shadow", style_ "width: 22rem;"] $ do
              div_ [class_ "card-body"] $ do
                h3_ [class_ "card-title text-center mb-4"] "Sign Up"
                form_ [action_ "/signup", method_ "post"] $ do
                  div_ [class_ "mb-3"] $ do
                    label_ [for_ "name", class_ "form-label"] "Full Name"
                    input_ [type_ "text", class_ "form-control", id_ "name", placeholder_ "Enter your full name", required_ "required", name_ "fullname"]
                  div_ [class_ "mb-3"] $ do
                    label_ [for_ "email", class_ "form-label"] "Email address"
                    input_ [type_ "email", class_ "form-control", id_ "email", placeholder_ "Enter your email", required_ "required", name_ "email"]
                  div_ [class_ "mb-3"] $ do
                    label_ [for_ "password", class_ "form-label"] "Password"
                    input_
                      [ type_ "password"
                      , class_ "form-control"
                      , id_ "password"
                      , placeholder_ "Enter your password"
                      , required_ "required"
                      , name_ "password"
                      -- , oninput_ "checkStrength()"
                      ]
                  -- div_ [class_ "strength-meter"] $do
                  --   div_ [id_ "strength-bar"] ""
                  --   small_ [id_ "strength-text", class_ "form-text"] ""
                  div_ [class_ "mb-3"] $ do
                    label_ [for_ "confirm-password", class_ "form-label"] "Confirm Password"
                    input_ [type_ "password", class_ "form-control", id_ "confirm-password", placeholder_ "Confirm your password", required_ "required", name_ "confirmpassword"]
                  when (isJust maybeWarning) $ div_ [class_ "text-danger mb-3"] (toHtml (fromMaybe "" maybeWarning))
                  div_ [class_ "d-grid"] $ do
                    button_ [type_ "submit", id_ "signup-button", class_ "btn btn-primary"] "Sign Up"
                p_ [class_ "text-center mt-3"] $ do
                  "Already have an account? "
                  a_ [href_ "/login"] "Login"
       in
        bootstrapHead "Sign up" extraHead bodyContent

--------------------------------------------------------------------------------
bootstrapHead :: PageTitle -> ExtraHeadContent -> BodyContent -> Html ()
bootstrapHead title extraHead content =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      title_ (toHtml title)
      link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css", rel_ "stylesheet", integrity_ "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH", crossorigin_ "anonymous"]
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"]
      -- script_ [src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"] ("" :: Text)
      extraHead
    body_ content
