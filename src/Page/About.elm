module Page.About exposing (view)

import Html as H
import Html.Attributes as HA
import Router exposing (Layout)
import Svg exposing (..)
import Svg.Attributes exposing (..)


valorInitA : Float
valorInitA =
    323.141


exampleCode : String
exampleCode =
    "import Browser\nimport Html exposing (Html, button, div, text)\nimport Html.Events exposing (onClick)\n\n\n\n-- MAIN\n\n\nmain =\n  Browser.sandbox { init = init, update = update, view = view }\n\n\n\n-- MODEL\n\ntype alias Model = Int\n\ninit : Model\ninit =\n  0\n\n\n-- UPDATE\n\ntype Msg = Increment | Decrement\n\nupdate : Msg -> Model -> Model\nupdate msg model =\n  case msg of\n    Increment ->\n      model + 1\n\n    Decrement ->\n      model - 1\n\n\n-- VIEW\n\nview : Model -> Html Msg\nview model =\n  div []\n    [ button [ onClick Decrement ] [ text ' - ' ]\n    , div [] [ text (String.fromInt model) ]\n    , button [ onClick Increment ] [ text ' + ' ]\n    ]"


exampleError : String
exampleError =
    "-- TYPE MISMATCH ---------------------------------------------------------------\n\nThe argument to function `toFullName` is causing a mismatch.\n\n6│   toFullName { fistName = ' Hermann ', lastName = ' Hesse ' }\n                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\nFunction `toFullName` is expecting the argument to be:\n\n    { …, firstName : … }\n\nBut it is:\n\n    { …, fistName : … }\n\nHint: I compared the record fields and found some potential typos.\n\n    firstName <-> fistName"


type alias ValorSSS =
    { key : Int
    , otherB : Float
    }


valorInitB : ValorSSS
valorInitB =
    { key = 81
    , otherB = 15.5
    }


view : Layout msg
view =
    { title = Just "About"
    , attrs = []
    , main =
        [ H.section [] [ H.text ("about page" ++ String.fromInt valorInitB.key) ]
        , svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 323.298 323.298" -- Ajustado para el tamaño original del contenido
            , width "100" -- Ajusta el ancho deseado
            , height "100" -- Ajusta la altura deseada
            ]
            [ polygon [ fill "#F0AD00", points "161.649,152.782 231.514,82.916 91.783,82.916" ] []
            , polygon [ fill "#7FD13B", points "8.867,0 79.241,70.375 232.213,70.375 161.838,0" ] []
            , rect
                [ fill "#7FD13B"
                , x "192.99"
                , y "107.392"
                , width "107.676"
                , height "108.167"
                , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
                ]
                []
            , polygon [ fill "#60B5CC", points "323.298,143.724 323.298,0 179.573,0" ] []
            , polygon [ fill "#5A6378", points "152.781,161.649 0,8.868 0,314.432" ] []
            , polygon [ fill "#F0AD00", points "255.522,246.655 323.298,314.432 323.298,178.879" ] []
            , polygon [ fill "#60B5CC", points "161.649,170.517 8.869,323.298 314.43,323.298" ] []
            ]
        , H.div []
            [ H.text "Elm es un lenguaje funcional que compila a JavaScript. Ayuda a hacer sitios web y aplicaciones web. Tiene un fuerte énfasis en la simplicidad y herramientas de calidad. Aparte de garantizar que si hacemos un proyecto con Elm veremos como terminamos escribiendo mejor codigo JavaScript."
            , H.text "Algunos beneficios de la programacion funcional"
            , H.br [] []
            , H.text "No hay errores en la practica"
            , H.text "Mensaje de error amables"
            , H.text "Reforzamiento confiable"
            , H.text "Version semantica aplicada para todos los paquetes Elm"
            , H.br [] []
            , H.text "Una función es una forma de transformar los valores. Tome un valor y producir otro."
            , H.br [] []
            , H.text "Values,Functions,If expressions, List,Tuples,Records"
            , H.br [] []
            , H.text "Hablemos de la arquitecttura de Elm.Los primeros programadores de Elm descubrieron patron basicos en elm, su arquitectura es facil , ademas de resultar bastante util en proyectos de fronted. *El punto es, incluso si finalmente no puedes usar Elm en el trabajo todavía, obtendrás mucho de usar Elm e internalizar este patrón.*"
            , H.img [ HA.src "https://guide.elm-lang.org/architecture/buttons.svg", HA.alt "ArquitecturaElm", HA.style "width" "500", HA.style "height" "600" ] []
            , H.text "Se compone de tres partes: "
            , H.br [] []
            , H.text "Model->(El estado de la aplicacion),View->(Convierte el estado en Html),Update->(Una forma de actualizar su estado en funcion de los mensajes)"
            ]
        , H.pre [ HA.style "white-space" "pre" ]
            [ H.text exampleCode ]
        , H.div [] [ H.text "1. Espere la entrada del usuario. \n2.Enviar un mensaje a update\n3.Producir un nuevo Model\n4.Llamada viewpara conseguir nuevo HTML\n5.Muestra el nuevo HTML en pantalla\n6.Repita.", H.br [] [], H.text "Esta es la esencia de La Arquitectura Elm." ]
        , H.div []
            [ H.text "TIPOS,Uno de los principales beneficios de Elm es que los usuarios no ven errores en el tiempo de ejecución en la práctica. Esto es posible porque el compilador Elm puede analizar su código fuente muy rápidamente para ver cómo fluen los valores a través de su programa. Si un valor puede ser utilizado de una manera inválida, el compilador le habla de ello con un mensaje de error amistoso. Esto se llama inferencia de tipo. El compilador calcula qué tipo de valores fluye dentro y fuera de todas sus funciones."
            , H.br [] []
            , H.pre [ HA.style "white-space" "pre" ]
                [ H.text exampleError ]
            , H.text "Es genial tener un asistente para simples errores como este, pero es aún más valioso cuando tienes cientos de archivos y un montón de colaboradores haciendo cambios. No importa cuán grandes y complejas se hagan las cosas, el compilador de Elm comprueba que todo encaja correctamente en base al código fuente."
            ]
        ]
    }
