module API.AuthApi where

import ClassyPrelude
import Servant.API
import Servant.Auth.Server
import Servant.HTML.Blaze
import Types (LoginForm, UserData)

type APIGoogleAuth = "google" :> Get '[JSON] NoContent

type APIGoogleAuthCb =
  "google" :> "cb" :> QueryParam "error" Text
    :> QueryParam "code" Text
    :> QueryParam "state" Text
    :> Get '[HTML] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UserData)

type AuthApi =
  APIGoogleAuth -- redirect User to the OpenID Provider
    :<|> APIGoogleAuthCb
    :<|> "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UserData)