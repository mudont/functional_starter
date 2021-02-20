module API.RajniApi where

import ClassyPrelude
import Servant.API
import Servant.Auth.Server
import Types

type RajniApi auths =
  Auth auths UserData :> "chuck" :> Get '[JSON] String
    :<|> "rajni" :> Get '[JSON] String