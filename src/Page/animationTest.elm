module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Time
import Debug
type Msg
    = Tick Time.Posix

type alias Model =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    , counter : Int
    , animationState : Bool 
    , reverse : Float
    }

initModel : () -> (Model, Cmd Msg)
initModel _ =
    ({ x1 = 100
    , y1 = 100
    , x2 = 200
    , y2 = 200
    , counter = 0
    , animationState = True
    , reverse = -1
    }
    , Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ ->
            ({ model
                | x1 = model.x1 + (-1 * -model.reverse)
                , y1 = model.y1 + (-1 * -model.reverse) 
                , x2 = model.x2 + (-1 * model.reverse)
                , y2 = model.y2 + (-1 * model.reverse)
                , counter = model.counter + 1
                , animationState = if ( modBy 100 model.counter  == 0) then True else False 
                , reverse = if model.animationState then model.reverse * -1 else model.reverse
            }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    Svg.svg
        [ SvgAttr.width "400" , SvgAttr.height "400" ]
        [ Svg.circle
            [ SvgAttr.cx (String.fromFloat model.x1) , SvgAttr.cy (String.fromFloat model.y1)
            , SvgAttr.r "20", SvgAttr.fill "blue"
            ]
            []
        , Svg.circle
            [ SvgAttr.cx (String.fromFloat model.x2), SvgAttr.cy (String.fromFloat model.y2)
            , SvgAttr.r "20", SvgAttr.fill "red"]
            []
        ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 10 Tick
main =
  Browser.element { init = initModel, update = update, subscriptions = subscriptions, view = view }
