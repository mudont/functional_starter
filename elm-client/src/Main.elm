module Main exposing (..)

import Browser
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Messages exposing (..)
import Model exposing (LoginState, Model)
import Subscriptions exposing (..)
import Update exposing (..)
import View exposing (..)


main : Program (Maybe LoginState) Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
