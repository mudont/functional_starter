module Api.Endpoint exposing (Endpoint, follow, login, googleLogin, register,
                              resetPassword, profiles, request, profile, users)

import Http
import Url.Builder exposing(..)
import Username exposing (Username)
import Email exposing (Email, toString)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , tracker : Maybe String
    }
    -> Cmd a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , tracker = config.tracker
        }



-- TYPES


{-| Get a URL to the Conduit API.

This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.

-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str

prodUrl = "https://mariandrive.com"
devUrl = "http://localhost:8080"
severUrl = devUrl
url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin severUrl
        ("api":: paths)
        queryParams
        |> Endpoint



-- ENDPOINTS


login : Endpoint
login =
    Url.Builder.crossOrigin severUrl ["login"] [] |> Endpoint -- url [ "login" ] []

googleLogin : Endpoint
googleLogin = Url.Builder.crossOrigin severUrl ["google"] [] |> Endpoint

register : Endpoint
register =
    Url.Builder.crossOrigin severUrl ["register"] [] |> Endpoint -- url [ "register" ] []

resetPassword : String -> Endpoint
resetPassword email =
    Url.Builder.crossOrigin severUrl ["reset_password"] [string "email"  email] |> Endpoint

profile : Username -> Endpoint
profile username =
    url [ "profile", Username.toString username ] []

--
--user : Endpoint
--user =
--    url [ "user" ] []


users : Endpoint
users =
    url [ "users" ] []


follow : Username -> Endpoint
follow uname =
    url [ "profiles", Username.toString uname, "follow" ] []




profiles : Username -> Endpoint
profiles uname =
    url [ "profiles", Username.toString uname ] []


