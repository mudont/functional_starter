module API.TennisApi where

import ClassyPrelude
import DB.Selda.CMModels hiding (email)
import Servant.API

type TennisApi =
  "users" :> Get '[JSON] [User]
    :<|> "user" :> Capture "user" Text :> Get '[JSON] (Maybe User)
    :<|> "players" :> Get '[JSON] [Player]
