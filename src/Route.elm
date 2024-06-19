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
    | NotFound Url


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map About (Parser.s "about")
        , Parser.map SecHome (Parser.s "SecHome")
        , Parser.map RegisterH (Parser.s "RegisterH")
        , Parser.map LoginH (Parser.s "LoginH")
        , Parser.map Contact (Parser.s "contact")
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

        Contact ->
            Just "Contact"

        NotFound _ ->
            Just "Not found"
