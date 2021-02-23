module View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import GoogleButton exposing (googleButton)
import Header.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Messages exposing (..)
import Model exposing (..)
import Util as Util



{-
   VIEW
   * Hide sections of view depending on authenticaton state of model
   * Get a quote
   * Log In or Register
   * Get a protected quote
-}


view : Model -> Browser.Document Msg
view model =
    let
        -- Is the user logged in?
        loggedIn : Bool
        loggedIn =
            String.length model.token > 0

        -- If the user is logged in, show a greeting; if logged out, show the login/register form
        authBoxView =
            let
                -- If there is an error on authentication, show the error alert
                showError : String
                showError =
                    if String.isEmpty model.errorMsg then
                        "hidden"

                    else
                        ""

                -- Greet a logged in user by username
                greeting : String
                greeting =
                    "Hello, " ++ Util.shortUsername model.username ++ "!"
            in
            if loggedIn then
                div [ id "greeting" ]
                    [ h3 [ class "text-center" ] [ text greeting ]
                    , p [ class "text-center" ] [ text "You have super-secret access to protected quotes." ]
                    , p [ class "text-center" ]
                        [ button [ class "btn btn-danger", onClick LogOut ] [ text "Log Out" ]
                        ]
                    ]

            else
                div [ id "form" ]
                    [ h2 [ class "text-center" ] [ text "Log In or Register" ]
                    , p [ class "help-block" ] [ text "If you already have an account, please Log In. Otherwise, enter your desired username and password and Register." ]
                    , div [ class showError ]
                        [ div [ class "alert alert-danger" ] [ text model.errorMsg ]
                        ]
                    , div [ class "form-group row" ]
                        [ div [ class "col-md-offset-2 col-md-8" ]
                            [ label [ for "username" ] [ text "Username:" ]
                            , input [ id "username", type_ "text", class "form-control", Html.Attributes.value model.username, onInput SetUsername ] []
                            ]
                        ]
                    , div [ class "form-group row" ]
                        [ div [ class "col-md-offset-2 col-md-8" ]
                            [ label [ for "password" ] [ text "Password:" ]
                            , input [ id "password", type_ "password", class "form-control", Html.Attributes.value model.password, onInput SetPassword ] []
                            ]
                        ]
                    , div [ class "text-center" ]
                        [ button [ class "btn btn-primary", onClick ClickLogIn ] [ text "Log In" ]
                        , button [ class "btn btn-link", onClick ClickRegisterUser ] [ text "Register" ]
                        , googleButton

                        --  , button [ class "btn btn-link", onClick ClickGoogleLogIn ] [ text "Google" ]
                        ]
                    ]
    in
    { title = "CM Hackers"
    , body =
        [ div [ class "container" ]
            [ Html.map HeaderMsg (Header.View.view model.header)
            , div [ class "jumbotron text-left" ]
                [ -- Login/Register form or user greeting
                  authBoxView
                ]
            , viewFooter model
            ]
        ]
    }


viewFooter : Model -> Html Msg
viewFooter model =
    let
        loggedIn =
            String.length model.token > 0

        -- If user is logged in, show button and quote; if logged out, show a message instructing them to log in
        protectedQuoteView =
            if loggedIn then
                div []
                    [ p [ class "text-center" ]
                        [ button [ class "btn btn-info", onClick GetProtectedQuote ] [ text "Chuck Norris" ]
                        ]
                    ]

            else
                p [ class "text-center" ] [ text "Log in for more" ]
    in
    div []
        [ Grid.row []
            [ Grid.col [] [ text "Random Quote" ]
            , Grid.col []
                [ p [ class "text-center" ]
                    [ button [ class "btn btn-success", onClick GetQuote ] [ text "Rajnikanth" ]
                    ]
                ]
            , Grid.col []
                [ div []
                    [ protectedQuoteView
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col
                []
                [ -- Blockquote with quote
                  blockquote []
                    [ p [] [ text model.quote ]
                    ]
                ]
            ]
        ]
