module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Loading
import Log
import Page
import PaginatedList exposing (PaginatedList)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)



-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , tab : HomeTab
    }


type HomeTab
    = Tab1 (Maybe Cred)
    | Tab2
    | Tab3

type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed



init : Session -> ( Model, Cmd Msg )
init session =

    ( { session = session
      , timeZone = Time.utc
      , tab = case Session.cred session of
                Just cred ->
                    Tab1 (Just cred)
                Nothing ->
                    Tab2
      }
    , Cmd.batch
        [ Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "CM Hackers"
    , content =
        div [ class "home-page" ]
            [ -- viewBanner,
              div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] <|
                                [ div [ class "feed-toggle" ] <|
                                    List.concat
                                        [ [ viewTabs
                                                [tab1 <| Session.cred model.session]
                                                tab2
                                                [tab3]
                                          ]
                                        ]
                                ]
                    ]
                ]
            ]
    }


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "CM Hackers" ]
            ]
        ]



-- TABS



tab1 : Maybe Cred -> ( String, Msg )
tab1 cred =
    ( "Events", ClickedTab (Tab1 cred) )


tab2 : ( String, Msg )
tab2 =
    ( "Players", ClickedTab Tab2 )


tab3 : ( String, Msg )
tab3  =
    ( "Matches" , ClickedTab (Tab3) )

viewTabs :
    List ( String, msg )
    -> ( String, msg )
    -> List ( String, msg )
    -> Html msg
viewTabs before selected after =
    ul [ class "nav nav-pills outline-active" ] <|
        List.concat
            [ List.map (viewTab []) before
            , [ viewTab [ class "active" ] selected ]
            , List.map (viewTab []) after
            ]

viewTab : List (Attribute msg) -> ( String, msg ) -> Html msg
viewTab attrs ( name, msg ) =
    li [ class "nav-item" ]
        [ -- Note: The RealWorld CSS requires an href to work properly.
          a (class "nav-link" :: onClick msg :: href "" :: attrs)
            [ text name ]
        ]


-- UPDATE


type Msg
    = ClickedTab HomeTab
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTab tab ->
            ( { model | tab = tab }
            , Cmd.none
            )
        _ ->
            ( model, Cmd.none )



-- HTTP


scrollToTop : Task x ()
scrollToTop =
    Dom.setViewport 0 0
        -- It's not worth showing the user anything special if scrolling fails.
        -- If anything, we'd log this to an error recording service.
        |> Task.onError (\_ -> Task.succeed ())



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
