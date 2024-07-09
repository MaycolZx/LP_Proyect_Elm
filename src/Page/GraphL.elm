module Page.GraphL exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Process
import Router exposing (Layout)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvents
import Time



-- MODEL


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Node =
    { id : Int
    , x : Float
    , y : Float
    }


type alias Edge =
    { from : Int
    , to : Int
    , weight : Int
    }


type alias Model =
    { nodes : Dict Int Node
    , edges : List Edge
    , nextId : Int
    , inputEdgeFrom : String
    , inputEdgeTo : String
    , inputEdgeWeight : String
    , selectedNodes : List Int
    , shortestPath : Maybe (List Int)
    , eulerianCycle : Maybe (List Int)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nodes = Dict.empty
      , edges = []
      , nextId = 1
      , inputEdgeFrom = ""
      , inputEdgeTo = ""
      , inputEdgeWeight = ""
      , selectedNodes = []
      , shortestPath = Nothing
      , eulerianCycle = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddNode Float Float
    | AddEdge
    | UpdateEdgeFromInput String
    | UpdateEdgeToInput String
    | UpdateEdgeWeightInput String
    | SelectNode Int
    | FindShortestPath
    | FindEulerianCycle
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNode x y ->
            let
                newNode =
                    { id = model.nextId
                    , x = x
                    , y = y
                    }
            in
            ( { model
                | nodes = Dict.insert model.nextId newNode model.nodes
                , nextId = model.nextId + 1
              }
            , Cmd.none
            )

        AddEdge ->
            case ( String.toInt model.inputEdgeFrom, String.toInt model.inputEdgeTo, String.toInt model.inputEdgeWeight ) of
                ( Just from, Just to, Just weight ) ->
                    if Dict.member from model.nodes && Dict.member to model.nodes then
                        ( { model
                            | edges = { from = from, to = to, weight = weight } :: model.edges
                            , inputEdgeFrom = ""
                            , inputEdgeTo = ""
                            , inputEdgeWeight = ""
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateEdgeFromInput input ->
            ( { model | inputEdgeFrom = input }, Cmd.none )

        UpdateEdgeToInput input ->
            ( { model | inputEdgeTo = input }, Cmd.none )

        UpdateEdgeWeightInput input ->
            ( { model | inputEdgeWeight = input }, Cmd.none )

        SelectNode id ->
            let
                newSelectedNodes =
                    if List.length model.selectedNodes < 2 then
                        id :: model.selectedNodes

                    else if List.member id model.selectedNodes then
                        List.filter (\n -> n /= id) model.selectedNodes

                    else
                        [ id ]
            in
            ( { model | selectedNodes = newSelectedNodes, shortestPath = Nothing }, Cmd.none )

        FindShortestPath ->
            case model.selectedNodes of
                [ start, end ] ->
                    ( { model | shortestPath = Just (dijkstra model.nodes model.edges start end) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FindEulerianCycle ->
            ( { model | eulerianCycle = findEulerianCycle model.nodes model.edges }, Cmd.none )

        Reset ->
            init ()



-- VIEW


view : Model -> Layout Msg
view model =
    let
        containerStyle =
            [ style "font-family" "Arial, sans-serif"
            , style "max-width" "800px"
            , style "margin" "20px auto"
            , style "padding" "20px"
            , style "background-color" "#f9f9f9"
            , style "border-radius" "8px"
            , style "box-shadow" "0 2px 4px rgba(0,0,0,0.1)"
            ]

        inputStyle =
            [ style "padding" "8px"
            , style "margin-right" "10px"
            , style "border" "1px solid #ccc"
            , style "border-radius" "4px"
            , style "font-size" "14px"
            , style "width" "150px"
            ]

        buttonStyle =
            [ style "padding" "8px 12px"
            , style "background-color" "#4CAF50"
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "4px"
            , style "cursor" "pointer"
            , style "font-size" "14px"
            , style "transition" "background-color 0.3s"
            ]

        inputGroupStyle =
            [ style "margin-bottom" "15px"
            , style "display" "flex"
            , style "align-items" "center"
            ]

        svgContainerStyle =
            [ style "border" "2px solid #ddd"
            , style "border-radius" "8px"
            , style "margin-top" "20px"
            , style "background-color" "#fff"
            ]
    in
    { title = Nothing
    , attrs = []
    , main =
        [ div containerStyle
            [ h1 [ style "color" "#333", style "text-align" "center" ] [ text "Visualizador de Grafo" ]
            , div [ style "text-align" "center", style "margin-bottom" "15px" ]
                [ text "Haz clic en el área del grafo para añadir nodos" ]
            , div inputGroupStyle
                [ input ([ type_ "text", placeholder "Nodo origen", value model.inputEdgeFrom, onInput UpdateEdgeFromInput ] ++ inputStyle) []
                , input ([ type_ "text", placeholder "Nodo destino", value model.inputEdgeTo, onInput UpdateEdgeToInput ] ++ inputStyle) []
                , input ([ type_ "text", placeholder "Peso de la arista", value model.inputEdgeWeight, onInput UpdateEdgeWeightInput ] ++ inputStyle) []
                , button (onClick AddEdge :: buttonStyle) [ text "Añadir Arista" ]
                ]
            , div inputGroupStyle
                [ button (onClick FindShortestPath :: buttonStyle) [ text "Encontrar camino más corto" ]
                , button (onClick Reset :: (buttonStyle ++ [ style "background-color" "#f44336" ])) [ text "Reiniciar" ]
                ]
            , div [ style "margin-top" "10px" ]
                [ text ("Nodos seleccionados: " ++ String.join ", " (List.map String.fromInt model.selectedNodes)) ]
            , case model.shortestPath of
                Just path ->
                    div [ style "margin-top" "10px", style "font-weight" "bold" ]
                        [ text ("Camino más corto: " ++ String.join " -> " (List.map String.fromInt path)) ]

                Nothing ->
                    text ""
            , div inputGroupStyle
                [ button (onClick FindEulerianCycle :: buttonStyle) [ text "Encontrar Ciclo Euleriano" ]
                ]
            , case model.eulerianCycle of
                Just cycle ->
                    div [ style "margin-top" "10px", style "font-weight" "bold" ]
                        [ text ("Ciclo Euleriano: " ++ String.join " -> " (List.map String.fromInt cycle)) ]

                Nothing ->
                    text "El grafo no tiene un ciclo euleriano"
            , div svgContainerStyle
                [ Svg.svg
                    [ SvgAttr.width "100%"
                    , SvgAttr.height "500"
                    , SvgAttr.viewBox "0 0 800 500"
                    , onSvgClick AddNode
                    ]
                    (List.concat
                        [ List.map (viewEdge model) model.edges
                        , List.map (viewNode model) (Dict.values model.nodes)
                        ]
                    )
                ]
            ]
        ]
    }


viewNode : Model -> Node -> Svg Msg
viewNode model node =
    let
        isSelected =
            List.member node.id model.selectedNodes

        isInPath =
            Maybe.withDefault False (Maybe.map (\path -> List.member node.id path) model.shortestPath)

        ( fillColor, strokeColor ) =
            if isSelected then
                ( "#e74c3c", "#c0392b" )
                -- Rojo

            else if isInPath then
                ( "#2ecc71", "#27ae60" )
                -- Verde

            else
                ( "#3498db", "#2980b9" )

        -- Azul
    in
    Svg.g []
        [ Svg.circle
            [ SvgAttr.cx (String.fromFloat node.x)
            , SvgAttr.cy (String.fromFloat node.y)
            , SvgAttr.r "20"
            , SvgAttr.fill fillColor
            , SvgAttr.stroke strokeColor
            , SvgAttr.strokeWidth "2"
            , SvgEvents.onClick (SelectNode node.id)
            , stopPropagationOn "click" (Decode.succeed ( SelectNode node.id, True ))
            ]
            []
        , Svg.text_
            [ SvgAttr.x (String.fromFloat node.x)
            , SvgAttr.y (String.fromFloat node.y)
            , SvgAttr.textAnchor "middle"
            , SvgAttr.dominantBaseline "central"
            , SvgAttr.fill "white"
            , SvgAttr.fontSize "14"
            , SvgAttr.fontWeight "bold"
            ]
            [ Svg.text (String.fromInt node.id) ]
        ]


viewEdge : Model -> Edge -> Svg Msg
viewEdge model edge =
    case ( Dict.get edge.from model.nodes, Dict.get edge.to model.nodes ) of
        ( Just fromNode, Just toNode ) ->
            let
                midX =
                    (fromNode.x + toNode.x) / 2

                midY =
                    (fromNode.y + toNode.y) / 2
            in
            Svg.g []
                [ Svg.line
                    [ SvgAttr.x1 (String.fromFloat fromNode.x)
                    , SvgAttr.y1 (String.fromFloat fromNode.y)
                    , SvgAttr.x2 (String.fromFloat toNode.x)
                    , SvgAttr.y2 (String.fromFloat toNode.y)
                    , SvgAttr.stroke "#34495e"
                    , SvgAttr.strokeWidth "2"
                    ]
                    []
                , Svg.rect
                    [ SvgAttr.x (String.fromFloat (midX - 10))
                    , SvgAttr.y (String.fromFloat (midY - 10))
                    , SvgAttr.width "20"
                    , SvgAttr.height "20"
                    , SvgAttr.fill "white"
                    , SvgAttr.stroke "#34495e"
                    ]
                    []
                , Svg.text_
                    [ SvgAttr.x (String.fromFloat midX)
                    , SvgAttr.y (String.fromFloat midY)
                    , SvgAttr.textAnchor "middle"
                    , SvgAttr.dominantBaseline "central"
                    , SvgAttr.fill "#34495e"
                    , SvgAttr.fontSize "12"
                    , SvgAttr.fontWeight "bold"
                    ]
                    [ Svg.text (String.fromInt edge.weight) ]
                ]

        _ ->
            Svg.g [] []



-- DECODIFICADOR DE CLICS


onSvgClick : (Float -> Float -> msg) -> Svg.Attribute msg
onSvgClick tagger =
    on "click" (Decode.map2 tagger (Decode.field "offsetX" Decode.float) (Decode.field "offsetY" Decode.float))


dijkstra : Dict Int Node -> List Edge -> Int -> Int -> List Int
dijkstra nodes edges start end =
    let
        infinity =
            1000000

        initialDistances =
            Dict.map
                (\k _ ->
                    if k == start then
                        0

                    else
                        infinity
                )
                nodes

        initialPrevious =
            Dict.empty

        unvisited =
            Dict.keys nodes

        getNeighbors nodeId =
            List.filter (\e -> e.from == nodeId || e.to == nodeId) edges
                |> List.map
                    (\e ->
                        if e.from == nodeId then
                            e.to

                        else
                            e.from
                    )

        getEdgeWeight from to =
            List.filter (\e -> (e.from == from && e.to == to) || (e.from == to && e.to == from)) edges
                |> List.head
                |> Maybe.map .weight
                |> Maybe.withDefault infinity

        findMinimumNode : List Int -> Dict Int Int -> Maybe Int
        findMinimumNode unvisitedNodes distances =
            List.foldl
                (\node acc ->
                    let
                        nodeDist =
                            Maybe.withDefault infinity (Dict.get node distances)
                    in
                    case acc of
                        Nothing ->
                            Just ( node, nodeDist )

                        Just ( minNode, minDist ) ->
                            if nodeDist < minDist then
                                Just ( node, nodeDist )

                            else
                                Just ( minNode, minDist )
                )
                Nothing
                unvisitedNodes
                |> Maybe.map Tuple.first

        loop : Dict Int Int -> Dict Int Int -> List Int -> ( Dict Int Int, Dict Int Int )
        loop distances previous unvisitedNodes =
            case unvisitedNodes of
                [] ->
                    ( distances, previous )

                _ ->
                    let
                        currentNode =
                            findMinimumNode unvisitedNodes distances

                        currentDistance =
                            Maybe.andThen (\n -> Dict.get n distances) currentNode
                    in
                    case ( currentNode, currentDistance ) of
                        ( Just node, Just dist ) ->
                            if node == end then
                                ( distances, previous )

                            else
                                let
                                    neighbors =
                                        getNeighbors node
                                            |> List.filter (\n -> List.member n unvisitedNodes)

                                    ( newDistances, newPrevious ) =
                                        List.foldl
                                            (\neighbor ( dists, prevs ) ->
                                                let
                                                    alt =
                                                        dist + getEdgeWeight node neighbor

                                                    currentDist =
                                                        Maybe.withDefault infinity (Dict.get neighbor dists)
                                                in
                                                if alt < currentDist then
                                                    ( Dict.insert neighbor alt dists
                                                    , Dict.insert neighbor node prevs
                                                    )

                                                else
                                                    ( dists, prevs )
                                            )
                                            ( distances, previous )
                                            neighbors
                                in
                                loop newDistances newPrevious (List.filter (\n -> n /= node) unvisitedNodes)

                        _ ->
                            ( distances, previous )

        ( _, finalPrevious ) =
            loop initialDistances initialPrevious unvisited

        buildPath : Int -> List Int -> List Int
        buildPath current path =
            if current == start then
                current :: path

            else
                case Dict.get current finalPrevious of
                    Just prev ->
                        buildPath prev (current :: path)

                    Nothing ->
                        path
    in
    buildPath end []



--Ciclo EULERIANO:


findEulerianCycle : Dict Int Node -> List Edge -> Maybe (List Int)
findEulerianCycle nodes edges =
    if isEulerian nodes edges then
        Just (findCycle nodes edges)

    else
        Nothing


isEulerian : Dict Int Node -> List Edge -> Bool
isEulerian nodes edges =
    let
        isConnected =
            isGraphConnected nodes edges

        allEvenDegree =
            Dict.values nodes
                |> List.all
                    (\node ->
                        modBy 2 (List.length (List.filter (\e -> e.from == node.id || e.to == node.id) edges)) == 0
                    )
    in
    isConnected && allEvenDegree


isGraphConnected : Dict Int Node -> List Edge -> Bool
isGraphConnected nodes edges =
    case Dict.values nodes of
        [] ->
            True

        first :: _ ->
            let
                visited =
                    dfs first.id nodes edges Dict.empty
            in
            Dict.size visited == Dict.size nodes


dfs : Int -> Dict Int Node -> List Edge -> Dict Int Bool -> Dict Int Bool
dfs nodeId nodes edges visited =
    case Dict.get nodeId nodes of
        Nothing ->
            visited

        Just node ->
            let
                newVisited =
                    Dict.insert nodeId True visited

                neighbors =
                    List.filter (\e -> e.from == nodeId || e.to == nodeId) edges
                        |> List.map
                            (\e ->
                                if e.from == nodeId then
                                    e.to

                                else
                                    e.from
                            )
                        |> List.filter (\n -> not (Dict.member n newVisited))
            in
            List.foldl (\n acc -> dfs n nodes edges acc) newVisited neighbors


findCycle : Dict Int Node -> List Edge -> List Int
findCycle nodes edges =
    case Dict.keys nodes of
        [] ->
            []

        start :: _ ->
            eulerianCycleHelper start edges []


eulerianCycleHelper : Int -> List Edge -> List Int -> List Int
eulerianCycleHelper current edges path =
    case List.filter (\e -> e.from == current || e.to == current) edges of
        [] ->
            List.reverse (current :: path)

        edge :: restEdges ->
            let
                next =
                    if edge.from == current then
                        edge.to

                    else
                        edge.from

                newEdges =
                    List.filter (\e -> e /= edge) edges
            in
            eulerianCycleHelper next newEdges (current :: path)
