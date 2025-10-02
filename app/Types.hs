module Types where

import Session.Backends.Postgres (DB)
import Session.Frontends.Scotty (ActionM)

type ActionS = ActionM DB
