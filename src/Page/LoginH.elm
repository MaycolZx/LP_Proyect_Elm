-- module SecHome exposing (main)


module Page.LoginH exposing (Model, Msg, init, update, view)

-- import Browser
-- import Html exposing (Html, button, div, text)
-- import Html.Events exposing (onClick)

import Html as H
import Html.Attributes as HA exposing (..)
import Html.Events as E
import Router exposing (Layout)



-- MODEL


type alias Model =
    { message : String }


init : Model
init =
    { message = "" }



-- MESSAGES


type Msg
    = ButtonClicked



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ButtonClicked ->
            { model | message = "Button was clicked!" }



-- VIEW
-- view : Model -> Html Msg
-- view model =
--     div []
--         [ button [ onClick ButtonClicked ] [ text "Click me" ]
--         , div [] [ text model.message ]
--         ]
-- view : Model -> (Msg -> msg) -> Html msg
-- view model toMsg =
--     div []
--         [ button [ onClick (toMsg ButtonClicked) ] [ text "Click me" ]
--         , div [] [ text model.message ]
--         ]


view : Model -> Layout Msg
view toMsgLog =
    { title = Nothing
    , attrs = []
    , main =
        [ H.div []
            [ H.div []
                [ H.h1 [ style "color" "red" ] [ H.text "Holaaaa" ]
                , H.button [ E.onClick ButtonClicked ] [ H.text "Click me" ]
                , H.div [] [ H.text toMsgLog.message ]
                ]
            , H.div []
                [ H.h2 [ style "color" "green" ] [ H.text "Controles" ]
                ]
            ]
        ]
    }



-- view : Layout msg
-- view =
--     { title = Nothing
--     , attrs = []
--     , main =
--         [ H.div []
--             -- [ H.button [ E.onClick (toMsg ButtonClicked) ] [ text "Click me" ]
--             [ H.button [] [ H.text "Click me" ]
--             , H.div [] [ H.text "HOOOOOOOOOOOo" ]
--             ]
--         ]
--     }
-- main : Program () Model Msg
-- main =
--     Browser.sandbox
--         { init = init
--         , view = view
--         , update = update
--         }
