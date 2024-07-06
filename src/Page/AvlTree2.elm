-- module Main exposing (..)


module Page.AvlTree2 exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Process
import Router exposing (Layout)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task



-- MAIN
-- main =
--     Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }
-- TREE


type BinaryTree
    = Nil
    | Node BinaryTree Int Int BinaryTree


heightT : BinaryTree -> Int
heightT tree =
    case tree of
        Nil ->
            0

        Node _ _ h _ ->
            h


updateHeight : BinaryTree -> BinaryTree
updateHeight tree =
    case tree of
        Nil ->
            Nil

        Node left value _ right ->
            Node left value (1 + Basics.max (heightT left) (heightT right)) right


balanceFactor : BinaryTree -> Int
balanceFactor tree =
    case tree of
        Nil ->
            0

        Node left _ _ right ->
            heightT left - heightT right


rotateLeft : BinaryTree -> ( BinaryTree, List Operation )
rotateLeft tree =
    case tree of
        Node left value h (Node rightLeft rightValue _ rightRight) ->
            ( Node (Node left value h rightLeft) rightValue (1 + Basics.max (heightT (Node left value h rightLeft)) (heightT rightRight)) rightRight, [ RotateLeft value ] )

        _ ->
            ( tree, [] )


rotateRight : BinaryTree -> ( BinaryTree, List Operation )
rotateRight tree =
    case tree of
        Node (Node leftLeft leftValue _ leftRight) value h right ->
            ( Node leftLeft leftValue (1 + Basics.max (heightT leftLeft) (heightT (Node leftRight value h right))) (Node leftRight value h right), [ RotateRight value ] )

        _ ->
            ( tree, [] )


balance : BinaryTree -> ( BinaryTree, List Operation )
balance tree =
    let
        ( balancedTree, ops ) =
            case tree of
                Nil ->
                    ( Nil, [] )

                Node left value h right ->
                    if balanceFactor tree > 1 then
                        if balanceFactor left < 0 then
                            let
                                ( newLeft, leftOps ) =
                                    rotateLeft left

                                ( newTree, rightOps ) =
                                    rotateRight (Node newLeft value h right)
                            in
                            ( newTree, leftOps ++ rightOps )

                        else
                            let
                                ( newTree, rotateOps ) =
                                    rotateRight tree
                            in
                            ( newTree, rotateOps )

                    else if balanceFactor tree < -1 then
                        if balanceFactor right > 0 then
                            let
                                ( newRight, rightOps ) =
                                    rotateRight right

                                ( newTree, leftOps ) =
                                    rotateLeft (Node left value h newRight)
                            in
                            ( newTree, rightOps ++ leftOps )

                        else
                            let
                                ( newTree, rotateOps ) =
                                    rotateLeft tree
                            in
                            ( newTree, rotateOps )

                    else
                        ( tree, [] )
    in
    ( updateHeight balancedTree, ops )


insert : Int -> BinaryTree -> ( BinaryTree, List Operation )
insert node tree =
    case tree of
        Nil ->
            ( Node Nil node 1 Nil, [ Insert node ] )

        Node left value h right ->
            if node < value then
                let
                    ( newLeft, leftOps ) =
                        insert node left

                    ( balancedTree, balanceOps ) =
                        balance (Node newLeft value h right)
                in
                ( balancedTree, leftOps ++ balanceOps )

            else if node > value then
                let
                    ( newRight, rightOps ) =
                        insert node right

                    ( balancedTree, balanceOps ) =
                        balance (Node left value h newRight)
                in
                ( balancedTree, rightOps ++ balanceOps )

            else
                ( tree, [] )



-- MODEL


type alias Model =
    { content : String
    , tree : BinaryTree
    , operations : List Operation
    , currentOperation : Maybe Operation
    , animating : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { content = ""
      , tree = Nil
      , operations = []
      , currentOperation = Nothing
      , animating = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String
    | AddToTree
    | NextOperation
    | StartAnimation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { model | content = newContent }
            , Cmd.none
            )

        AddToTree ->
            case String.toInt model.content of
                Just val ->
                    let
                        ( newTree, ops ) =
                            insert val model.tree
                    in
                    ( { model | tree = newTree, content = "", operations = ops, currentOperation = Nothing, animating = True }
                    , Cmd.batch [ Process.sleep 500 |> Task.perform (\_ -> NextOperation) ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        NextOperation ->
            case model.operations of
                [] ->
                    ( { model | animating = False }
                    , Cmd.none
                    )

                op :: ops ->
                    ( { model | currentOperation = Just op, operations = ops }
                    , Process.sleep 500 |> Task.perform (\_ -> NextOperation)
                    )

        StartAnimation ->
            ( model
            , Process.sleep 500 |> Task.perform (\_ -> NextOperation)
            )



-- VIEW


view : Model -> Layout Msg
view model =
    { title = Nothing
    , attrs = []
    , main =
        [ div []
            [ input [ placeholder "Enter number", value model.content, onInput Change ] []
            , button [ onClick AddToTree ] [ Html.text "Add to Tree" ]
            , div [] [ renderTree model.tree model.currentOperation ]
            , if model.animating then
                button [ onClick NextOperation ] [ Html.text "Next Operation" ]

              else
                Html.text ""
            ]
        ]
    }



-- div []
--     [ input [ placeholder "Enter number", value model.content, onInput Change ] []
--     , button [ onClick AddToTree ] [ Html.text "Add to Tree" ]
--     , div [] [ renderTree model.tree model.currentOperation ]
--     , if model.animating then
--         button [ onClick NextOperation ] [ Html.text "Next Operation" ]
--
--       else
--         Html.text ""
--     ]
-- DRAW TREE


type Operation
    = Insert Int
    | RotateLeft Int
    | RotateRight Int


drawTree : BinaryTree -> Maybe Operation -> Float -> Float -> Float -> List (Svg msg)
drawTree tree currentOperation x y offset =
    case tree of
        Nil ->
            []

        Node left leaf _ right ->
            drawLines tree x y offset
                ++ drawNodes tree currentOperation x y offset


drawLines : BinaryTree -> Float -> Float -> Float -> List (Svg msg)
drawLines tree x y offset =
    case tree of
        Nil ->
            []

        Node left leaf _ right ->
            drawLine ( x, y ) ( x - offset, y + 60 )
                ++ drawLine ( x, y ) ( x + offset, y + 60 )
                ++ drawLines left (x - offset) (y + 60) (offset / 2)
                ++ drawLines right (x + offset) (y + 60) (offset / 2)


drawNodes : BinaryTree -> Maybe Operation -> Float -> Float -> Float -> List (Svg msg)
drawNodes tree currentOperation x y offset =
    case tree of
        Nil ->
            []

        Node left leaf _ right ->
            [ drawNode x y leaf currentOperation ]
                ++ drawNodes left currentOperation (x - offset) (y + 60) (offset / 2)
                ++ drawNodes right currentOperation (x + offset) (y + 60) (offset / 2)


drawNode : Float -> Float -> Int -> Maybe Operation -> Svg msg
drawNode x y leaf currentOperation =
    let
        color =
            case currentOperation of
                Just (Insert n) ->
                    if n == leaf then
                        "green"

                    else
                        "red"

                Just (RotateLeft n) ->
                    if n == leaf then
                        "blue"

                    else
                        "red"

                Just (RotateRight n) ->
                    if n == leaf then
                        "blue"

                    else
                        "red"

                Nothing ->
                    "red"
    in
    g []
        [ circle [ cx (String.fromFloat x), cy (String.fromFloat y), r "20", fill color ] []
        , text_ [ Svg.Attributes.x (String.fromFloat (x - 10)), Svg.Attributes.y (String.fromFloat (y + 5)), fill "white", fontSize "15" ] [ Svg.text (String.fromInt leaf) ]
        ]


drawLine : ( Float, Float ) -> ( Float, Float ) -> List (Svg msg)
drawLine ( x1, y1 ) ( x2, y2 ) =
    [ line [ Svg.Attributes.x1 (String.fromFloat x1), Svg.Attributes.y1 (String.fromFloat y1), Svg.Attributes.x2 (String.fromFloat x2), Svg.Attributes.y2 (String.fromFloat y2), stroke "black", strokeWidth "2" ] [] ]


renderTree : BinaryTree -> Maybe Operation -> Svg msg
renderTree tree currentOperation =
    svg [ width "800", height "600" ] (drawTree tree currentOperation 400 50 200)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
