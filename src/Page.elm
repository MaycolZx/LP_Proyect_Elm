module Page exposing (Msg, Page(..), init, subscriptions, update, view)

import Page.About as About
import Page.Contact as Contact
import Page.Home as Home
import Page.LoginH as LoginH
import Page.NotFound as NotFound
import Page.RegisterH as RegisterH
import Page.SecHome as SecHome
import Route exposing (Route(..))
import Router exposing (Layout)
import Url exposing (Url)


{-| Page
-}
type Page
    = Home Home.Model
    | About
    | Contact Contact.Model
    | SecHome SecHome.Model
    | RegisterH RegisterH.Model
    | LoginH LoginH.Model
    | NotFound Url


{-| Msg
-}
type Msg
    = ContactMsg Contact.Msg
    | SecHomeMsg SecHome.Msg
    | RegisterHMsg RegisterH.Msg
    | LoginHMsg LoginH.Msg
    | HomeMsg Home.Msg


{-| init
-}
init : Route -> ( Page, Cmd Msg )
init route =
    case route of
        Route.Home ->
            ( Home Home.init, Cmd.none )

        Route.About ->
            ( About, Cmd.none )

        Route.Contact ->
            Contact.init "" ""
                |> Router.mapUpdate Contact ContactMsg

        Route.SecHome ->
            ( SecHome SecHome.init, Cmd.none )

        Route.RegisterH ->
            ( RegisterH RegisterH.init, Cmd.none )

        Route.LoginH ->
            ( LoginH LoginH.init, Cmd.none )

        -- SecHome.init ""
        --     |> Router.mapUpdate SecHome SecHomeMsg
        Route.NotFound url ->
            ( NotFound url, Cmd.none )


{-| update
-}
update : Msg -> Page -> ( Page, Cmd Msg )
update message page =
    case message of
        ContactMsg msg ->
            case page of
                Contact mdl ->
                    Contact.update msg mdl
                        |> Router.mapUpdate Contact ContactMsg

                _ ->
                    ( page, Cmd.none )

        SecHomeMsg msg ->
            case page of
                SecHome mdl ->
                    ( SecHome (SecHome.update msg mdl), Cmd.none )

                _ ->
                    ( page, Cmd.none )

        RegisterHMsg msg ->
            case page of
                RegisterH mdl ->
                    ( RegisterH (RegisterH.update msg mdl), Cmd.none )

                _ ->
                    ( page, Cmd.none )

        LoginHMsg msg ->
            case page of
                LoginH mdl ->
                    ( LoginH (LoginH.update msg mdl), Cmd.none )

                _ ->
                    ( page, Cmd.none )

        HomeMsg msg ->
            case page of
                Home mdl ->
                    ( Home (Home.update msg mdl), Cmd.none )

                _ ->
                    ( page, Cmd.none )



-- SecHomeMsg msg ->
--     case page of
--         SecHome mdl ->
--             ( SecHome (SecHome.update msg mdl), Cmd.none )
--         -- RegisterH mdl ->
--         --     ( RegisterH (RegisterH.update RegisterHMsg mdl), Cmd.none )
--         -- LoginH mdl ->
--         --     ( LoginH (LoginH.update msg mdl), Cmd.none )
--         _ ->
--             ( page, Cmd.none )
-- RegisterHMsg msga ->
--     case page of
--         RegisterH mdl ->
--             ( RegisterH (RegisterH.update msga mdl), Cmd.none )
--         _ ->
--             ( page, Cmd.none )
-- LoginHMsg msgl ->
--     case page of
--         LoginH mdl ->
--             ( LoginH (LoginH.update msgl mdl), Cmd.none )
--         _ ->
--             ( page, Cmd.none )


{-| view
-}
view : Page -> Layout Msg
view page =
    case page of
        Home mdl ->
            Home.view mdl
                |> Router.mapView HomeMsg

        About ->
            About.view

        Contact mdl ->
            Contact.view mdl
                |> Router.mapView ContactMsg

        SecHome mdl ->
            SecHome.view mdl
                |> Router.mapView SecHomeMsg

        RegisterH mdl ->
            RegisterH.view mdl
                |> Router.mapView RegisterHMsg

        LoginH mdl ->
            LoginH.view mdl
                |> Router.mapView LoginHMsg

        NotFound url ->
            NotFound.view url


{-| subscriptions
-}
subscriptions : Page -> Sub Msg
subscriptions page =
    case page of
        Home _ ->
            Sub.none

        About ->
            Sub.none

        Contact mdl ->
            Contact.subscriptions mdl
                |> Sub.map ContactMsg

        SecHome _ ->
            Sub.none

        RegisterH _ ->
            Sub.none

        LoginH _ ->
            Sub.none

        NotFound _ ->
            Sub.none
