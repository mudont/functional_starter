module API.TennisApi where

import           ClassyPrelude
import           DB.Selda.CMModels   hiding (email)
import           Servant.API
import           Servant.Auth.Server
import           Types

type TennisApi auths =
  "users" :> Get '[JSON] [User]
    :<|> "user" :> Capture "user" Text :> Get '[JSON] (Maybe User)
    :<|> Auth auths UserData :> "players" :> Get '[JSON] [ContactInfo]
    :<|> Auth auths UserData :> "profile" :> ReqBody '[JSON] ContactInfo :> Put '[JSON] ()
    :<|> Auth auths UserData :> "profile" :> Capture "user" Text :> Get '[JSON] (Maybe ContactInfo)
