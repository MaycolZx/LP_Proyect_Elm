module Page.Home exposing (view)

import Html as H
import Html.Attributes as HA exposing (..)
import Router exposing (Layout, url)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)


styledDiv : List (H.Html msg) -> H.Html msg
styledDiv children =
    H.div [ HA.style "width" "500px", HA.style "height" "500px", HA.style "display" "flex", HA.style "justify-content" "center", HA.style "align-items" "center", HA.style "border" "5px solid black" ] children


animatedSVG : H.Html msg
animatedSVG =
    S.svg [ SA.width "120", SA.height "120", SA.viewBox "0 0 120 120" ] [ circle [ cx "50", cy "50", r "6" ] [] ]


otherDiv : String -> String -> H.Html msg
otherDiv url titleCard =
    let
        urlG =
            "https://visualgo.net/img/"
    in
    H.div []
        [ H.a []
            [ H.img [ HA.src (String.concat [ urlG, "gif/", url, ".gif" ]), HA.alt "please wait" ]
                []
            , H.h2
                []
                [ H.text titleCard ]
            , H.div [ HA.style "cursor" "pointer", HA.style "opacity" "1", HA.style "backface-visibility" "hidden", HA.style "background-image" (String.concat [ "url(", urlG, "png/", url, ".png)" ]) ] [ H.text "oo" ]
            ]
        ]



-- S.svg [ HA.style "width" "100px", HA.style "height" "50px", HA.class "animated-svg" ]
--     [ H.text
--         [ HA.style "font-size" "24px", HA.style "fill" "black", HA.style "opacity" "0" ]
--         [ H.text "Animated Text" ]
--     ]


view : Layout msg
view =
    { title = Nothing
    , attrs = [ HA.style "background-color" "#f3edff" ]
    , main =
        [ H.div [ HA.style "margin" "20px" ]
            -- H.div [ HA.style "display" "grid", HA.style "grid-template-rows" "1fr 5fr 1fr ", HA.style "margin" "20px" ]
            [ H.h1 [ HA.style "display" "flex", HA.style "justify-content" "center", HA.style "align-items" "center" ] [ H.text "VISU algo 2.0" ]
            , H.div [ HA.style "display" "grid", HA.style "grid-template-columns" "1fr 1fr", HA.style "gap" "20px" ]
                [ styledDiv [ otherDiv "sorting" "Primera D S" ]
                , styledDiv [ otherDiv "list" "Segunda D S" ]
                , styledDiv [ otherDiv "heap" "Tercer D S" ]
                , styledDiv [ otherDiv "hashtable" "Cuarta D S" ]
                , styledDiv [ otherDiv "bst" "Quinta D S" ]
                , styledDiv [ otherDiv "graphds" "Sexta D S" ]
                ]
            ]
        ]
    }
