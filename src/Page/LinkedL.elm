module Page.LinkedL exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html as H exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Process
import Router exposing (Layout)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Time



-- MODELO


type alias Model =
    { linkedList : List LinkedListNode
    , inputValue : String
    , searchValue : String
    , nextId : Int
    }


type alias LinkedListNode =
    { id : Int
    , value : String
    , x : Float -- Posición X actual
    , targetX : Float -- Posición X objetivo
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { linkedList = []
      , inputValue = ""
      , searchValue = ""
      , nextId = 0
      }
    , Cmd.none
    )



-- ACTUALIZACIÓN


type Msg
    = AddNode
    | RemoveNode
    | UpdateInput String
    | UpdateSearch String
    | RemoveByValue
    | Tick Time.Posix
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNode ->
            if String.isEmpty model.inputValue then
                ( model, Cmd.none )

            else
                let
                    newNode =
                        { id = model.nextId
                        , value = model.inputValue
                        , x = -100
                        , targetX = -100
                        }

                    newList =
                        insertSorted newNode model.linkedList
                in
                ( { model
                    | linkedList = List.indexedMap (\index e -> { e | targetX = getTargetX index }) newList
                    , inputValue = ""
                    , nextId = model.nextId + 1
                  }
                , Cmd.none
                )

        RemoveNode ->
            case model.linkedList of
                [] ->
                    ( model, Cmd.none )

                _ :: rest ->
                    ( { model | linkedList = reorderList rest }
                    , Cmd.none
                    )

        UpdateInput newValue ->
            ( { model | inputValue = newValue }, Cmd.none )

        UpdateSearch newSearchValue ->
            ( { model | searchValue = newSearchValue }, Cmd.none )

        RemoveByValue ->
            let
                newList =
                    model.linkedList
                        |> List.filter (\node -> node.value /= model.searchValue)
                        |> reorderList
            in
            ( { model | linkedList = newList }
            , Cmd.none
            )

        Tick _ ->
            ( { model | linkedList = List.map updateNodePosition model.linkedList }, Cmd.none )

        Reset ->
            init ()


insertSorted : LinkedListNode -> List LinkedListNode -> List LinkedListNode
insertSorted newNode list =
    let
        compareValues a b =
            case ( String.toInt a, String.toInt b ) of
                ( Just numA, Just numB ) ->
                    numA <= numB

                _ ->
                    a <= b

        -- Fallback a comparación de strings si la conversión falla
    in
    case list of
        [] ->
            [ newNode ]

        head :: tail ->
            if compareValues newNode.value head.value then
                newNode :: list

            else
                head :: insertSorted newNode tail


reorderList : List LinkedListNode -> List LinkedListNode
reorderList list =
    list
        |> List.sortBy
            (\node ->
                case String.toInt node.value of
                    Just num ->
                        num

                    Nothing ->
                        0
            )
        |> List.indexedMap
            (\index node ->
                { node | targetX = getTargetX index }
            )



--To calculate the position it will get To.


getTargetX : Int -> Float
getTargetX index =
    50 + (toFloat index * 150)


updateNodePosition : LinkedListNode -> LinkedListNode
updateNodePosition node =
    let
        step =
            (node.targetX - node.x) / 10

        -- Animation speed
        newX =
            if abs (node.x - node.targetX) < 1 then
                node.targetX

            else
                node.x + step
    in
    { node | x = newX }



-- VISTA


view : Model -> Layout Msg
view model =
    let
        inputStyle =
            [ HA.style "padding" "8px"
            , HA.style "margin-right" "10px"
            , HA.style "border" "1px solid #ccc"
            , HA.style "border-radius" "4px"
            , HA.style "font-size" "14px"
            ]

        buttonStyle =
            [ HA.style "padding" "8px 12px"
            , HA.style "background-color" "#4CAF50"
            , HA.style "color" "white"
            , HA.style "border" "none"
            , HA.style "border-radius" "4px"
            , HA.style "cursor" "pointer"
            , HA.style "font-size" "14px"
            , HA.style "transition" "background-color 0.3s"
            ]

        resetButtonStyle =
            buttonStyle ++ [ HA.style "background-color" "#e74c3c" ]

        containerStyle =
            [ HA.style "font-family" "Arial, sans-serif"
            , HA.style "max-width" "800px"
            , HA.style "margin" "20px auto"
            , HA.style "padding" "20px"
            , HA.style "background-color" "#f9f9f9"
            , HA.style "border-radius" "8px"
            , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.1)"
            ]

        inputContainerStyle =
            [ HA.style "margin-bottom" "20px"
            , HA.style "display" "flex"
            , HA.style "align-items" "center"
            ]
    in
    { title = Nothing
    , attrs = []
    , main =
        [ div containerStyle
            [ h1 [ HA.style "color" "#333", HA.style "text-align" "center" ] [ H.text "Visualizador de Lista enlazada" ]
            , div inputContainerStyle
                [ input ([ type_ "text", placeholder "Valor del nodo", value model.inputValue, onInput UpdateInput ] ++ inputStyle) []
                , button (onClick AddNode :: buttonStyle) [ H.text "Agregar Nodo" ]
                ]
            , div inputContainerStyle
                [ input ([ type_ "text", placeholder "Valor del nodo a eliminar", value model.searchValue, onInput UpdateSearch ] ++ inputStyle) []
                , button (onClick RemoveByValue :: buttonStyle) [ H.text "Eliminar Nodo por Valor" ]
                ]
            , div inputContainerStyle
                [ button (onClick Reset :: resetButtonStyle) [ H.text "Reiniciar" ]
                ]
            , svg
                [ SvgAttr.width "100%"
                , SvgAttr.height "300"
                , SvgAttr.viewBox "0 0 2000 300"
                ]
                (List.concatMap drawLinkedListNode model.linkedList)
            ]
        ]
    }


drawLinkedListNode : LinkedListNode -> List (Svg Msg)
drawLinkedListNode node =
    let
        rectX =
            node.x

        rectY =
            100

        rectWidth =
            100

        rectHeight =
            100

        arrowX1 =
            node.x + rectWidth

        arrowY1 =
            rectY + rectHeight / 2

        arrowX2 =
            arrowX1 + 50
    in
    [ rect
        [ SvgAttr.x (String.fromFloat rectX)
        , SvgAttr.y (String.fromFloat rectY)
        , SvgAttr.width (String.fromFloat rectWidth)
        , SvgAttr.height (String.fromFloat rectHeight)
        , SvgAttr.fill "#3498db"
        , SvgAttr.rx "10"
        , SvgAttr.ry "10"
        , SvgAttr.stroke "#2980b9"
        , SvgAttr.strokeWidth "2"
        ]
        []
    , text_
        [ SvgAttr.x (String.fromFloat (rectX + rectWidth / 2))
        , SvgAttr.y (String.fromFloat (rectY + rectHeight / 2))
        , SvgAttr.textAnchor "middle"
        , SvgAttr.fill "white"
        , SvgAttr.dominantBaseline "middle"
        , SvgAttr.fontSize "32"
        , SvgAttr.fontWeight "bold"
        ]
        [ Svg.text node.value ]
    , line
        [ SvgAttr.x1 (String.fromFloat arrowX1)
        , SvgAttr.y1 (String.fromFloat arrowY1)
        , SvgAttr.x2 (String.fromFloat arrowX2)
        , SvgAttr.y2 (String.fromFloat arrowY1)
        , SvgAttr.stroke "#34495e"
        , SvgAttr.strokeWidth "2"
        ]
        []
    , polygon
        [ SvgAttr.points
            (String.join ","
                [ String.fromFloat arrowX2 ++ " " ++ String.fromFloat (arrowY1 - 5)
                , String.fromFloat (arrowX2 + 10) ++ " " ++ String.fromFloat arrowY1
                , String.fromFloat arrowX2 ++ " " ++ String.fromFloat (arrowY1 + 5)
                ]
            )
        , SvgAttr.fill "#34495e"
        ]
        []
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 16 Tick
