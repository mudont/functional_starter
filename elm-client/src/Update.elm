port module Update exposing (..)

import Header.Update
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Messages exposing (..)
import Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HeaderMsg headerMsg ->
            let
                ( headerModel, headerCmd ) =
                    Header.Update.update headerMsg model.header
            in
            ( { model | header = headerModel }
            , Cmd.map HeaderMsg headerCmd
            )

        GetQuote ->
            ( model, fetchRandomQuoteCmd )

        FetchRandomQuoteCompleted result ->
            fetchRandomQuoteCompleted model result

        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        ClickRegisterUser ->
            ( model, authUserCmd model registerUrl )

        ClickLogIn ->
            ( model, authUserCmd model loginUrl )

        ClickGoogleLogIn ->
            ( model, googleLoginCmd )

        GetTokenCompleted result ->
            getTokenCompleted model result

        GetProtectedQuote ->
            ( model, fetchProtectedQuoteCmd model )

        FetchProtectedQuoteCompleted result ->
            fetchProtectedQuoteCompleted model result

        LogOut ->
            ( { model | username = "", token = "" }, removeStorage <| toStorage model )



-- POST register / login request


authUserCmd : Model -> String -> Cmd Msg
authUserCmd model apiUrl =
    let
        body =
            model
                |> userEncoder
                |> Http.jsonBody
    in
    Http.post { url = apiUrl, body = body, expect = Http.expectJson GetTokenCompleted tokenDecoder2 }


getTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok newToken ->
            setStorageHelper { model | token = newToken, password = "", errorMsg = "" }

        Err _ ->
            ( { model | errorMsg = " Some Http.Error. Don't know how to display" }, Cmd.none )



-- Decode POST response to get access token
-- Should be called to process reponse of successful login
--


tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "access_token" Decode.string


tokenDecoder2 : Decoder String
tokenDecoder2 =
    Decode.field "accessToken" Decode.string



-- GET request for random protected quote (authenticated)


fetchProtectedQuoteCmd : Model -> Cmd Msg
fetchProtectedQuoteCmd model =
    { method = "GET"
    , headers = [ Http.header "Authorization" ("Bearer " ++ model.token) ]
    , url = protectedQuoteUrl
    , body = Http.emptyBody
    , expect = Http.expectString FetchProtectedQuoteCompleted
    , timeout = Nothing
    , tracker = Nothing

    -- , withCredentials = False
    }
        |> Http.request


fetchProtectedQuoteCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
fetchProtectedQuoteCompleted model result =
    case result of
        Ok newPQuote ->
            setStorageHelper { model | quote = newPQuote }

        Err _ ->
            ( model, Cmd.none )



-- Helper to update model and set localStorage with the updated model


setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model =
    ( model, setStorage <| toStorage model )



-- API request URLs


api : String
api =
    "/"


protectedQuoteUrl : String
protectedQuoteUrl =
    api ++ "chuck"


randomQuoteUrl : String
randomQuoteUrl =
    api ++ "rajni"


registerUrl : String
registerUrl =
    api ++ "register"


loginUrl : String
loginUrl =
    api ++ "login"


googleLoginUrl : String
googleLoginUrl =
    api ++ "google"



-- Encode user to construct POST request body (for Register and Log In)


userEncoder : Model -> Encode.Value
userEncoder model =
    Encode.object
        [ ( "username", Encode.string model.username )
        , ( "password", Encode.string model.password )
        ]


init : Maybe LoginState -> ( Model, Cmd Msg )
init model =
    let
        ( headerState, headerCmd ) =
            Header.Update.init
    in
    case model of
        Just m ->
            ( { header = headerState
              , username = m.username
              , password = m.password
              , token = m.token
              , quote = m.quote
              , errorMsg = m.errorMsg
              }
            , Cmd.batch
                [ Cmd.map HeaderMsg headerCmd
                , fetchRandomQuoteCmd
                ]
            )

        Nothing ->
            ( { header = headerState
              , username = ""
              , password = ""
              , token = ""
              , quote = ""
              , errorMsg = ""
              }
            , Cmd.batch
                [ Cmd.map HeaderMsg headerCmd
                , fetchRandomQuoteCmd
                ]
            )



-- GET a random quote (unauthenticated)


googleLoginCmd : Cmd Msg
googleLoginCmd =
    Http.get
        { url = googleLoginUrl
        , expect = Http.expectString FetchRandomQuoteCompleted
        }


fetchRandomQuoteCmd : Cmd Msg
fetchRandomQuoteCmd =
    Http.get
        { url = randomQuoteUrl
        , expect = Http.expectString FetchRandomQuoteCompleted
        }


fetchRandomQuoteCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
fetchRandomQuoteCompleted model result =
    case result of
        Ok newQuote ->
            setStorageHelper { model | quote = newQuote }

        Err _ ->
            ( model, Cmd.none )



-- Ports


toStorage : Model -> LoginState
toStorage m =
    { username = m.username
    , password = m.password
    , token = m.token
    , quote = m.errorMsg
    , errorMsg = m.errorMsg
    }


port setStorage : LoginState -> Cmd msg


port removeStorage : LoginState -> Cmd msg
