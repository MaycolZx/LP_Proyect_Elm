-- module Page.Home exposing (view)


module Page.Home exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA exposing (..)
import Html.Events as HE exposing (..)
import Router exposing (Layout, url)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)



-- import Browser
-- import Html exposing (Html)
-- styledDiv : List (H.Html msg) -> H.Html msg
-- styledDiv children =
--     H.div [ HA.style "width" "500px", HA.style "height" "500px", HA.style "display" "flex", HA.style "justify-content" "center", HA.style "align-items" "center", HA.style "border" "5px solid black" ] children


animatedSVG : H.Html msg
animatedSVG =
    S.svg [ SA.width "120", SA.height "120", SA.viewBox "0 0 120 120" ] [ circle [ cx "50", cy "50", r "6" ] [] ]



-- main =
--     Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { hoveredDiv : Maybe String }


init : Model
init =
    { hoveredDiv = Nothing }


type Msg
    = MouseEnter String
    | MouseLeave String


update : Msg -> Model -> Model
update msg toHome =
    case msg of
        MouseEnter id ->
            { toHome | hoveredDiv = Just id }

        MouseLeave id ->
            { toHome | hoveredDiv = Nothing }


styledDiv : String -> String -> Maybe String -> H.Html Msg
styledDiv id titleCard hoveredDiv =
    let
        isHovered =
            case hoveredDiv of
                Just hoveredId ->
                    hoveredId == id

                Nothing ->
                    False
    in
    H.div [ HA.style "margin" "0 auto" ]
        [ otherDiv id titleCard isHovered ]


otherDiv : String -> String -> Bool -> H.Html Msg
otherDiv id titleCard isHovered =
    let
        urlG =
            "https://visualgo.net/img/"
    in
    H.a
        [ HA.href "/RegisterH"
        , HA.style "color" "black"
        , HA.style "cursor" "pointer"
        , HA.style "width" "300px"
        , HA.style "height" "300px"
        , HA.style "border" "1px solid #000"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"

        -- , HA.style "justify-content" "center"
        -- , HA.style "align-items" "center"
        , HA.style "overflow" "hidden"
        , onMouseEnter (MouseEnter id)
        , onMouseLeave (MouseLeave id)
        ]
        [ if isHovered then
            H.img
                [ HA.style "width" "100%"
                , HA.style "height" "100%"
                , HA.style "object-fit" "contain"
                , src (String.concat [ urlG, "gif/", id, ".gif" ])
                , alt "Animated-Image"
                ]
                []

          else
            H.img
                [ HA.style "width" "100%"
                , HA.style "height" "100%"
                , HA.style "object-fit" "contain"
                , src (String.concat [ "./assets/images/", id, ".png" ])
                , alt "Static-Image"
                ]
                []
        , H.h2 [ HA.style "font-family" "Roboto,sans-serif", HA.style "margin" "0 auto" ] [ H.text titleCard ]
        ]



-- otherDiv : String -> String -> H.Html msg
-- otherDiv url titleCard =
--     let
--         urlG =
--             "https://visualgo.net/img/"
--     in
--     H.div []
--         [ H.a [ HA.style "width" "300px", HA.style "height" "300px", HA.style "border" "1px solid #000", HA.style "display" "flex", HA.style "justify-content" "center", HA.style "align-items" "center" ]
--             [ H.img [ HA.style "display" "block", HA.style "", HA.src "./assets/images/bst.png", HA.alt "Static-Image" ] []
--             , H.img [ HA.style "display" "none", HA.src (String.concat [ urlG, "gif/", url, ".gif" ]), HA.alt "please wait" ]
--                 []
--             , H.h2
--                 []
--                 [ H.text titleCard ]
--             , H.div [ HA.style "cursor" "pointer", HA.style "opacity" "1", HA.style "backface-visibility" "hidden", HA.style "background-image" "./assets/images/bst.png" ] [ H.text "oo" ]
--             ]
--         ]
-- S.svg [ HA.style "width" "100px", HA.style "height" "50px", HA.class "animated-svg" ]
--     [ H.text
--         [ HA.style "font-size" "24px", HA.style "fill" "black", HA.style "opacity" "0" ]
--         [ H.text "Animated Text" ]
--     ]


view : Model -> Layout Msg
view toHome =
    { title = Nothing
    , attrs = [ HA.style "background-color" "#f3edff" ]
    , main =
        [ H.div []
            -- H.div [ HA.style "display" "grid", HA.style "grid-template-rows" "1fr 5fr 1fr ", HA.style "margin" "20px" ]
            [ H.h1 [ HA.style "display" "flex", HA.style "justify-content" "center", HA.style "align-items" "center", HA.style "margin-top" "0", HA.style "margin-bottom" "0" ] [ H.text "VISU algo 2.0" ]
            , H.div [ HA.style "display" "grid", HA.style "grid-template-columns" "1fr 1fr", HA.style "gap" "20px" ]
                [ styledDiv "sorting" "Primera D S" toHome.hoveredDiv
                , styledDiv "list" "Segunda D S" toHome.hoveredDiv
                , styledDiv "heap" "Tercer D S" toHome.hoveredDiv
                , styledDiv "hashtable" "Cuarta D S" toHome.hoveredDiv
                , styledDiv "bst" "Quinta D S" toHome.hoveredDiv
                , styledDiv "graphds" "Sexta D S" toHome.hoveredDiv
                ]
            ]
        ]
    }
