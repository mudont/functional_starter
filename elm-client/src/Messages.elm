module Messages exposing (..)

import Header.Messages
import Http


type Msg
    = HeaderMsg Header.Messages.Msg
    | GetQuote
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
