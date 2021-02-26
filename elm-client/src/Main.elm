module Main exposing (..)

import Browser
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Login.Model
import Messages exposing (..)
import Model exposing (Model)
import Subscriptions exposing (..)
import Update exposing (..)
import View exposing (..)


main : Program Login.Model.Model Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        --, onUrlRequest = LinkClicked
        --, onUrlChange = UrlChanged
        }

