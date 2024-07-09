module Page.QueueL exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Router exposing (Layout)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Time


type alias Model =
    { queue : List QueueElement
    , inputVal : String
    , nextId : Int
    , highlightFront : Bool
    , highlightBack : Bool
    }



-- x -> coord
-- targetX -> destiny coord


type alias QueueElement =
    { id : Int
    , value : String
    , x : Float
    , targetX : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { queue = []
      , inputVal = ""
      , nextId = 0
      , highlightFront = False
      , highlightBack = False
      }
    , Cmd.none
    )


type Msg
    = Enqueue
    | Dequeue
    | UpdateInput String
    | Tick Time.Posix
    | HighlightFront
    | HighlightBack


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Enqueue ->
            if String.isEmpty model.inputVal then
                ( model, Cmd.none )

            else
                let
                    newElem =
                        { id = model.nextId
                        , value = model.inputVal

                        --Just referential coord to begin in
                        , x = -100

                        --Goes at the end of queue
                        , targetX = getTargetX (List.length model.queue)
                        }

                    newQueue =
                        model.queue ++ [ newElem ]
                in
                ( { model
                    | queue = List.indexedMap (\index e -> { e | targetX = getTargetX index }) newQueue
                    , inputVal = ""
                    , nextId = model.nextId + 1
                  }
                , Cmd.none
                )

        Dequeue ->
            case model.queue of
                [] ->
                    ( model, Cmd.none )

                _ :: rest ->
                    let
                        newQueue =
                            List.indexedMap (\index e -> { e | targetX = getTargetX index }) rest
                    in
                    ( { model
                        | queue = newQueue
                      }
                    , Cmd.none
                    )

        UpdateInput newValue ->
            ( { model | inputVal = newValue }, Cmd.none )

        Tick _ ->
            ( { model | queue = List.map updateElementPosition model.queue }, Cmd.none )

        HighlightFront ->
            ( { model | highlightFront = not model.highlightFront, highlightBack = False }, Cmd.none )

        HighlightBack ->
            ( { model | highlightBack = not model.highlightBack, highlightFront = False }, Cmd.none )



--Adjust here for space between elements


getTargetX : Int -> Float
getTargetX index =
    50 + (toFloat index * 110)


updateElementPosition : QueueElement -> QueueElement
updateElementPosition elem =
    let
        step =
            5

        newX =
            if abs (elem.x - elem.targetX) < step then
                elem.targetX

            else if elem.x < elem.targetX then
                elem.x + step

            else
                elem.x - step
    in
    { elem | x = newX }


view : Model -> Layout Msg
view model =
    let
        containerStyle =
            [ Html.Attributes.style "font-family" "Arial, sans-serif"
            , Html.Attributes.style "max-width" "900px"
            , Html.Attributes.style "margin" "20px auto"
            , Html.Attributes.style "padding" "20px"
            , Html.Attributes.style "background-color" "#f9f9f9"
            , Html.Attributes.style "border-radius" "8px"
            , Html.Attributes.style "box-shadow" "0 2px 4px rgba(0,0,0,0.1)"
            ]

        inputStyle =
            [ Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "margin-right" "10px"
            , Html.Attributes.style "border" "1px solid #ccc"
            , Html.Attributes.style "border-radius" "4px"
            , Html.Attributes.style "font-size" "14px"
            ]

        buttonStyle =
            [ Html.Attributes.style "padding" "8px 12px"
            , Html.Attributes.style "background-color" "#4CAF50"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "border-radius" "4px"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "font-size" "14px"
            , Html.Attributes.style "margin-right" "5px"
            , Html.Attributes.style "transition" "background-color 0.3s"
            ]

        queueContainerStyle =
            [ Html.Attributes.style "border" "2px solid #ddd"
            , Html.Attributes.style "border-radius" "8px"
            , Html.Attributes.style "margin-top" "20px"
            , Html.Attributes.style "padding" "10px"
            , Html.Attributes.style "background-color" "#fff"
            ]

        infoStyle =
            [ Html.Attributes.style "margin-top" "10px"
            , Html.Attributes.style "font-size" "16px"
            , Html.Attributes.style "color" "#333"
            ]
    in
    { title = Nothing
    , attrs = []
    , main =
        [ div containerStyle
            [ h1 [ Html.Attributes.style "color" "#333", Html.Attributes.style "text-align" "center" ] [ Html.text "Visualizador de Cola" ]
            , div []
                [ input ([ type_ "text", placeholder "Agregar elemento", value model.inputVal, onInput UpdateInput ] ++ inputStyle) []
                , button (onClick Enqueue :: buttonStyle) [ Html.text "Enqueue" ]
                , button (onClick Dequeue :: buttonStyle) [ Html.text "Dequeue" ]
                , button (onClick HighlightFront :: buttonStyle) [ Html.text "Front" ]
                , button (onClick HighlightBack :: buttonStyle) [ Html.text "Back" ]
                ]
            , div queueContainerStyle
                [ svg
                    [ SvgAttr.width "100%"
                    , SvgAttr.height "250" -- Aumentado para acomodar elementos más grandes
                    , SvgAttr.viewBox "0 0 1500 250" -- Ajustado para elementos más grandes
                    ]
                    (List.indexedMap (drawQueueElement model) model.queue)
                ]
            , div infoStyle [ Html.text ("Elementos en la cola: " ++ String.fromInt (List.length model.queue)) ]
            ]
        ]
    }


drawQueueElement : Model -> Int -> QueueElement -> Svg Msg
drawQueueElement model index elem =
    let
        squareSize =
            80.0

        -- Aumentado de 60 a 80
        halfSquareSize =
            squareSize / 2.0

        textX =
            elem.x + halfSquareSize

        textY =
            100

        -- Ajustado para centrar en el cuadrado más grande
        isFront =
            index == 0

        isBack =
            index == List.length model.queue - 1

        fillColor =
            if (isFront && model.highlightFront) || (isBack && model.highlightBack) then
                "#e74c3c"
                -- Un rojo más suave

            else
                "#3498db"

        -- Un azul agradable
    in
    g []
        [ rect
            [ SvgAttr.x (String.fromFloat elem.x)
            , SvgAttr.y "50"
            , SvgAttr.width (String.fromFloat squareSize)
            , SvgAttr.height (String.fromFloat squareSize)
            , SvgAttr.fill fillColor
            , SvgAttr.rx "10" -- Bordes redondeados ajustados
            , SvgAttr.ry "10"
            , SvgAttr.stroke "#2980b9"
            , SvgAttr.strokeWidth "2"
            ]
            []
        , text_
            [ SvgAttr.x (String.fromFloat textX)
            , SvgAttr.y (String.fromFloat textY)
            , SvgAttr.textAnchor "middle"
            , SvgAttr.fill "white"

            --Change here for element Font
            , SvgAttr.fontSize "26"
            , SvgAttr.fontWeight "bold"
            ]
            [ Svg.text elem.value ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 16 Tick
