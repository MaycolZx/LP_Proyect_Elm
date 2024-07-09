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
        [ H.tbody [ style "display" "flex", style "flex-direction" "column", style "min-height" "100vh", style "margin" "0" ]
            [ H.nav [ style "display" "grid", HA.style "grid-template-columns" "3fr 1fr 1fr", style "background" "black", style "color" "white" ]
                [ H.a
                    [ HA.href "/"
                    , HA.style "width" "45px"
                    , HA.style "height" "45px"
                    ]
                    [ H.img
                        [ HA.style "width" "100%"
                        , HA.style "height" "100%"
                        , HA.style "object-fit" "contain"
                        , HA.src "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8f/Orange_lambda.svg/1959px-Orange_lambda.svg.png"
                        , HA.alt "Logo"
                        ]
                        []
                    ]
                , H.a [ HA.style "text-decoration" "none", HA.style "font-family" "Roboto,sans-serif", HA.href "/RegisterH", HA.style "border" "black", HA.style "background-color" "purple", HA.style "color" "white", HA.style "margin" "0 auto", HA.style "padding" "15px" ] [ H.text "Register" ]
                , H.a [ HA.style "text-decoration" "none", HA.style "font-family" "Roboto,sans-serif", HA.href "/LoginH", HA.style "color" "white", HA.style "margin" "0 auto", HA.style "padding" "15px" ] [ H.text "Login" ]

                -- , H.a [ HA.href "/something_not_routed" ] [ H.text "404" ]
                ]
            , H.main_ (layout.attrs ++ [ style "flex" "1" ]) layout.main
            , H.footer []
                [ H.div [ HA.style "display" "flex", HA.style "flex-flow" "row wrap", HA.style "justify-content" "space-between", HA.style "padding" "12px 60px", HA.style "margin-top" "20px", HA.style "background-color" "black", HA.style "text-align" "right", HA.style "line-height" "normal", HA.style "color" "white" ]
                    [ H.a [ HA.style "color" "white", HA.style "text-decoration" "none", HA.href "/about" ] [ H.text "About" ]
                    , H.a [ HA.style "color" "white", HA.style "text-decoration" "none", HA.href "/contact" ] [ H.text "Terms of use" ]
                    , H.a [ HA.style "color" "white", HA.style "text-decoration" "none", HA.href "/SecHome" ] [ H.text "Privacy Policy" ]
                    ]
                ]
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
