module Page.LoginH exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA exposing (..)
import Html.Events as E
import Router exposing (Layout)



-- MODEL


type alias Model =
    { username : String
    , password : String
    , message : String
    }


init : Model
init =
    { username = ""
    , password = ""
    , message = ""
    }



-- MESSAGES


type Msg
    = UpdateUsername String
    | UpdatePassword String
    | SubmitLogin
    | LoginSuccess
    | LoginFailure String



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateUsername username ->
            { model | username = username }

        UpdatePassword password ->
            { model | password = password }

        SubmitLogin ->
            if model.username == "admin" && model.password == "password" then
                { model | message = "Login successful!" }

            else
                { model | message = "Invalid username or password." }

        LoginSuccess ->
            { model | message = "Login successful!" }

        LoginFailure errorMsg ->
            { model | message = errorMsg }



-- VIEW


view : Model -> Layout Msg
view model =
    { title = Nothing
    , attrs = []
    , main =
        [ H.div []
            [ H.h1 [ style "color" "black" ] [ H.text "Login" ]
            , H.div []
                [ H.label [] [ H.text "Username:" ]
                , H.input [ type_ "text", value model.username, E.onInput UpdateUsername ] []
                ]
            , H.div []
                [ H.label [] [ H.text "Password:" ]
                , H.input [ type_ "password", value model.password, E.onInput UpdatePassword ] []
                ]
            , H.button [ E.onClick SubmitLogin ] [ H.text "Login" ]
            , H.div [] [ H.text model.message ]
            ]
        ]
    }
