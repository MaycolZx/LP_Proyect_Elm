module Page exposing (Msg, Page(..), init, subscriptions, update, view)

import Page.About as About
import Page.AvlTree2 as AvlTree2
import Page.Contact as Contact
import Page.GraphL as GraphL
import Page.Home as Home
import Page.LinkedL as LinkedL
import Page.LoginH as LoginH
import Page.NotFound as NotFound
import Page.QueueL as QueueL
import Page.RegisterH as RegisterH
import Page.SecHome as SecHome
import Page.StackL as StackL
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
    | AvlTree2 AvlTree2.Model
    | NotFound Url
    | LinkedL LinkedL.Model
    | GraphL GraphL.Model
    | QueueL QueueL.Model
    | StackL StackL.Model


{-| Msg
-}
type Msg
    = ContactMsg Contact.Msg
    | SecHomeMsg SecHome.Msg
    | RegisterHMsg RegisterH.Msg
    | LoginHMsg LoginH.Msg
    | HomeMsg Home.Msg
    | AvlTree2Msg AvlTree2.Msg
    | LinkedLMsg LinkedL.Msg
    | GraphLMsg GraphL.Msg
    | QueueLMsg QueueL.Msg
    | StackLMsg StackL.Msg


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

        Route.AvlTree2 ->
            let
                ( model, cmd ) =
                    AvlTree2.init ()
            in
            ( AvlTree2 model, Cmd.map AvlTree2Msg cmd )

        Route.LinkedL ->
            let
                ( model, cmd ) =
                    LinkedL.init ()
            in
            ( LinkedL model, Cmd.map LinkedLMsg cmd )

        Route.GraphL ->
            let
                ( model, cmd ) =
                    GraphL.init ()
            in
            ( GraphL model, Cmd.map GraphLMsg cmd )

        Route.QueueL ->
            let
                ( model, cmd ) =
                    QueueL.init ()
            in
            ( QueueL model, Cmd.map QueueLMsg cmd )

        Route.StackL ->
            let
                ( model, cmd ) =
                    StackL.init ()
            in
            ( StackL model, Cmd.map StackLMsg cmd )

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

        AvlTree2Msg msg ->
            case page of
                AvlTree2 mdl ->
                    let
                        ( newModel, cmd ) =
                            AvlTree2.update msg mdl
                    in
                    ( AvlTree2 newModel, Cmd.map AvlTree2Msg cmd )

                _ ->
                    ( page, Cmd.none )

        LinkedLMsg msg ->
            case page of
                LinkedL mdl ->
                    let
                        ( newModel, cmd ) =
                            LinkedL.update msg mdl
                    in
                    ( LinkedL newModel, Cmd.map LinkedLMsg cmd )

                _ ->
                    ( page, Cmd.none )

        GraphLMsg msg ->
            case page of
                GraphL mdl ->
                    let
                        ( newModel, cmd ) =
                            GraphL.update msg mdl
                    in
                    ( GraphL newModel, Cmd.map GraphLMsg cmd )

                _ ->
                    ( page, Cmd.none )

        QueueLMsg msg ->
            case page of
                QueueL mdl ->
                    let
                        ( newModel, cmd ) =
                            QueueL.update msg mdl
                    in
                    ( QueueL newModel, Cmd.map QueueLMsg cmd )

                _ ->
                    ( page, Cmd.none )

        StackLMsg msg ->
            case page of
                StackL mdl ->
                    let
                        ( newModel, cmd ) =
                            StackL.update msg mdl
                    in
                    ( StackL newModel, Cmd.map StackLMsg cmd )

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

        AvlTree2 mdl ->
            AvlTree2.view mdl
                |> Router.mapView AvlTree2Msg

        LinkedL mdl ->
            LinkedL.view mdl
                |> Router.mapView LinkedLMsg

        GraphL mdl ->
            GraphL.view mdl
                |> Router.mapView GraphLMsg

        QueueL mdl ->
            QueueL.view mdl
                |> Router.mapView QueueLMsg

        StackL mdl ->
            StackL.view mdl
                |> Router.mapView StackLMsg

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

        AvlTree2 mdl ->
            AvlTree2.subscriptions mdl
                |> Sub.map AvlTree2Msg

        LinkedL mdl ->
            LinkedL.subscriptions mdl
                |> Sub.map LinkedLMsg

        GraphL mdl ->
            GraphL.subscriptions mdl
                |> Sub.map GraphLMsg

        QueueL mdl ->
            QueueL.subscriptions mdl
                |> Sub.map QueueLMsg

        StackL mdl ->
            StackL.subscriptions mdl
                |> Sub.map StackLMsg

        NotFound _ ->
            Sub.none
