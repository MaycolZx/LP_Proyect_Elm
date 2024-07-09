module Route exposing (Route(..), name, parser)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)



-- MODEL


type Route
    = Home
    | About
    | Contact
    | SecHome
    | RegisterH
    | LoginH
    | AvlTree2
    | NotFound Url
    | LinkedL
    | GraphL
    | QueueL
    | StackL


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map About (Parser.s "about")
        , Parser.map SecHome (Parser.s "SecHome")
        , Parser.map RegisterH (Parser.s "RegisterH")
        , Parser.map LoginH (Parser.s "LoginH")
        , Parser.map AvlTree2 (Parser.s "AvlTree2")
        , Parser.map Contact (Parser.s "contact")
        , Parser.map LinkedL (Parser.s "LinkedL")
        , Parser.map GraphL (Parser.s "GraphL")
        , Parser.map QueueL (Parser.s "QueueL")
        , Parser.map StackL (Parser.s "StackL")
        ]



-- TITLE


name : Route -> Maybe String
name route =
    case route of
        Home ->
            Nothing

        About ->
            Just "About"

        SecHome ->
            Just "Sechome"

        RegisterH ->
            Just "RegisterH"

        LoginH ->
            Just "LoginH"

        AvlTree2 ->
            Just "AvlTree2"

        LinkedL ->
            Just "LinkedL"

        GraphL ->
            Just "GraphL"

        QueueL ->
            Just "QueueL"

        StackL ->
            Just "StackL"

        Contact ->
            Just "Contact"

        NotFound _ ->
            Just "Not found"
