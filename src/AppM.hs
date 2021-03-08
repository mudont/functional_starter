module AppM where

import           ClassyPrelude
import           Config
import           Database.Selda.Backend    (SeldaConnection)
import           Database.Selda.PostgreSQL
import           Servant                   as S
import           Types

data AppState = AppState
  { cfg     :: AppConfig,
    oidcEnv :: OIDCEnv,
    dbConn  :: SeldaConnection PG
  }

type AppM = ReaderT AppState S.Handler
