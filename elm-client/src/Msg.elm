module Msg exposing (..)

import Http


type Msg
    = GetQuote
    | FetchRandomQuoteCompleted (Result Http.Error String)
    | SetUsername String
    | SetPassword String
    | ClickRegisterUser
    | ClickLogIn
    | GetTokenCompleted (Result Http.Error String)
    | GetProtectedQuote
    | FetchProtectedQuoteCompleted (Result Http.Error String)
    | ClickGoogleLogIn
    | LogOut
