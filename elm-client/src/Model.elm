module Model exposing (..)

import Header.Model exposing (..)
import Messages exposing (..)


type alias LoginState =
    { username : String
    , password : String
    , token : String
    , quote : String
    , errorMsg : String
    }


type alias Header_ a =
    { a | header : Header.Model.Model }


type alias Model =
    Header_ LoginState
