module API.TennisApi where

import ClassyPrelude
import DB.Models hiding (email)
import Servant.API

type TennisApi =
  "users" :> Get '[JSON] [User]
    :<|> "user" :> Capture "user" Text :> Get '[JSON] (Maybe User)
    :<|> "groups" :> Get '[JSON] [Group]
