module Page.SecHome exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA exposing (..)
import Router exposing (Layout)



-- MODEL


type alias Model =
    { message : String }


init : Model
init =
    { message = "" }



-- MESSAGES


type Msg
    = NoOp



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


view : Model -> Layout Msg
view model =
    { title = Just "Política de Privacidad"
    , attrs = []
    , main =
        [ H.div []
            [ H.h1 [ style "color" "black", style "text-align" "center" ] [ H.text "Política de Privacidad" ]
            , H.div []
                [ H.p []
                    [ H.text "Esta política de privacidad describe cómo recopilamos, usamos y protegemos su información personal. Al utilizar nuestros servicios, usted acepta las prácticas descritas en esta política."
                    ]
                , H.h2 [] [ H.text "Recopilación de Información" ]
                , H.p []
                    [ H.text "Recopilamos información personal que usted nos proporciona voluntariamente al registrarse en nuestros servicios, suscribirse a nuestro boletín, completar formularios o interactuar con nosotros de cualquier otra manera. La información puede incluir su nombre, dirección de correo electrónico, número de teléfono y cualquier otra información que decida proporcionarnos."
                    ]
                , H.h2 [] [ H.text "Uso de la Información" ]
                , H.p []
                    [ H.text "Usamos la información recopilada para proporcionar y mejorar nuestros servicios, comunicarnos con usted, enviarle actualizaciones y promociones, y personalizar su experiencia en nuestro sitio web. No compartimos su información personal con terceros sin su consentimiento, excepto cuando sea necesario para cumplir con la ley o proteger nuestros derechos."
                    ]
                , H.h2 [] [ H.text "Protección de la Información" ]
                , H.p []
                    [ H.text "Implementamos medidas de seguridad adecuadas para proteger su información personal contra el acceso no autorizado, la alteración, divulgación o destrucción. Sin embargo, ninguna transmisión de datos por Internet o almacenamiento electrónico es completamente segura, por lo que no podemos garantizar la seguridad absoluta de su información."
                    ]
                , H.h2 [] [ H.text "Cambios en la Política de Privacidad" ]
                , H.p []
                    [ H.text "Nos reservamos el derecho de actualizar esta política de privacidad en cualquier momento. Cualquier cambio será publicado en esta página y la fecha de actualización se reflejará en la parte superior de la política. Le recomendamos revisar esta política periódicamente para mantenerse informado sobre cómo protegemos su información."
                    ]
                , H.h2 [] [ H.text "Contacto" ]
                , H.p []
                    [ H.text "Si tiene alguna pregunta o inquietud sobre nuestra política de privacidad, no dude en contactarnos a través de nuestro formulario de contacto o enviándonos un correo electrónico a privacy@example.com."
                    ]
                ]
            ]
        ]
    }
