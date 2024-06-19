module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html as H
import Html.Attributes as HA exposing (style)
import Page exposing (Page(..))
import Route exposing (Route)
import Router exposing (Config, Router)
import Url exposing (Url)



-- MODEL


type alias Model =
    { router : Router Route Page }



-- MSG


type Msg
    = RouterMsg (Router.Msg Page.Msg)



-- ROUTER CONFIG


config : Config Msg Route Page Page.Msg
config =
    { init = Page.init
    , update = Page.update
    , view = Page.view
    , subscriptions = Page.subscriptions
    , bind = RouterMsg
    , parser = Route.parser
    , notFound = Route.NotFound
    , options = Router.defaultOptions
    }



-- INIT


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( router, cmd ) =
            Router.init config url key
    in
    ( Model router, cmd )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update message ({ router } as model) =
    case message of
        RouterMsg msg ->
            let
                ( newRouter, cmd ) =
                    Router.update config msg router
            in
            ( { model | router = newRouter }, cmd )



-- VIEW


view : Model -> Document Msg
view { router } =
    let
        layout =
            Router.view config router
    in
    { title =
        layout.title
            |> Maybe.map (\pageTitle -> "Simple - " ++ pageTitle)
            |> Maybe.withDefault "Simple"
    , body =
        [ H.nav [ style "display" "grid", style "background" "black", style "color" "white" ]
            [ H.a [ HA.href "/" ] [ H.text "Home" ]
            , H.a [ HA.href "/RegisterH" ] [ H.text "Register" ]
            , H.a [ HA.href "/LoginH" ] [ H.text "Login" ]

            -- , H.a [ HA.href "/something_not_routed" ] [ H.text "404" ]
            ]
        , H.main_ layout.attrs layout.main
        , H.div [ HA.style "padding" "12px 60px", HA.style "margin-top" "20px", HA.style "background-color" "black", HA.style "text-align" "right", HA.style "line-height" "normal", HA.style "color" "white" ]
            [ H.a [ HA.href "/about" ] [ H.text "About" ]
            , H.a [ HA.href "/SecHome" ] [ H.text "Team" ]
            , H.a [ HA.href "/contact" ] [ H.text "Terms of use" ]
            , H.a [ HA.href "/SecHome" ] [ H.text "Privacy Policy" ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { router } =
    Router.subscriptions config router



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = Router.onUrlChange RouterMsg
        , onUrlRequest = Router.onUrlRequest RouterMsg
        }
