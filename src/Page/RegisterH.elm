module Page.RegisterH exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA exposing (..)
import Html.Events as E
import Router exposing (Layout)



-- MODEL


type alias Model =
    { message : String
    , email : String
    , name : String
    , nickname : String
    , phone : String
    , profileImage : String
    , age : String
    , password : String
    , confirmPassword : String
    , acceptTerms : Bool
    , acceptConditions : Bool
    }


init : Model
init =
    { message = ""
    , email = ""
    , name = ""
    , nickname = ""
    , phone = ""
    , profileImage = ""
    , age = ""
    , password = ""
    , confirmPassword = ""
    , acceptTerms = False
    , acceptConditions = False
    }



-- MESSAGES


type Msg
    = ButtonClicked
    | UpdateEmail String
    | UpdateName String
    | UpdateNickname String
    | UpdatePhone String
    | UpdateProfileImage String
    | UpdateAge String
    | UpdatePassword String
    | UpdateConfirmPassword String
    | ToggleAcceptTerms Bool
    | ToggleAcceptConditions Bool



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ButtonClicked ->
            { model | message = "Button was clicked!" }

        UpdateEmail email ->
            { model | email = email }

        UpdateName name ->
            { model | name = name }

        UpdateNickname nickname ->
            { model | nickname = nickname }

        UpdatePhone phone ->
            { model | phone = phone }

        UpdateProfileImage profileImage ->
            { model | profileImage = profileImage }

        UpdateAge age ->
            { model | age = age }

        UpdatePassword password ->
            { model | password = password }

        UpdateConfirmPassword confirmPassword ->
            { model | confirmPassword = confirmPassword }

        ToggleAcceptTerms accept ->
            { model | acceptTerms = accept }

        ToggleAcceptConditions accept ->
            { model | acceptConditions = accept }



-- VIEW


view : Model -> Layout Msg
view model =
    { title = Nothing
    , attrs = []
    , main =
        [ H.div []
            [ H.h1 [ HA.style "color" "red" ] [ H.text "Registro" ]
            , H.form []
                [ H.div []
                    [ H.label [] [ H.text "Correo Electrónico:" ]
                    , H.input [ type_ "email", value model.email, E.onInput UpdateEmail ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Nombre:" ]
                    , H.input [ type_ "text", value model.name, E.onInput UpdateName ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Nickname:" ]
                    , H.input [ type_ "text", value model.nickname, E.onInput UpdateNickname ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Número de Celular:" ]
                    , H.input [ type_ "tel", value model.phone, E.onInput UpdatePhone ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Imagen de Perfil:" ]
                    , H.input [ type_ "file", value model.profileImage, E.onInput UpdateProfileImage ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Edad:" ]
                    , H.input [ type_ "number", value model.age, E.onInput UpdateAge ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Contraseña:" ]
                    , H.input [ type_ "password", value model.password, E.onInput UpdatePassword ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Confirmar Contraseña:" ]
                    , H.input [ type_ "password", value model.confirmPassword, E.onInput UpdateConfirmPassword ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Aceptar Términos:" ]
                    , H.input [ type_ "checkbox", checked model.acceptTerms, E.onCheck ToggleAcceptTerms ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Aceptar Condiciones:" ]
                    , H.input [ type_ "checkbox", checked model.acceptConditions, E.onCheck ToggleAcceptConditions ] []
                    ]
                , H.button [ E.onClick ButtonClicked ] [ H.text "Registrarse" ]
                , H.div [] [ H.text model.message ]
                ]
            ]
        ]
    }
