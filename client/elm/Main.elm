module Main exposing (Model, Msg, main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, a, button, footer, h1, header, main_, map, p, section, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Page.Dashboard as Dashboard
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Proof as Proof
import Port.Blockstack as Blockstack
import Project
import Random.Pcg.Extended exposing (initialSeed)
import Router
import Session
import Skeleton
import Svg exposing (g, svg)
import Svg.Attributes
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)



-- MAIN


main : Program Flag Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Flag =
    ( Int, List Int )


type alias Model =
    { page : Router.Page, key : Nav.Key, user : Session.User, projects : Project.Model, sidebar : Skeleton.Model }



-- @TODO: check authentication throwing command on init


init : Flag -> Url -> Nav.Key -> ( Model, Cmd Msg )
init ( seed, seedExtension ) url navKey =
    let
        model =
            { key = navKey
            , page = Router.route url
            , user = ( Session.Anonymous, Nothing )
            , projects = ( Project.emptyProject, [], initialSeed seed seedExtension )
            , sidebar = Skeleton.Closed
            }
    in
    ( model, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Session.subscriptions model.user
            |> Sub.map SessionMsg
            |> Sub.map Forward
        , Project.subscriptions
            |> Sub.map ProjectMsg
            |> Sub.map Forward
        ]


view : Model -> Browser.Document Msg
view model =
    let
        document =
            case model.page of
                Router.Home ->
                    { title = Home.title
                    , body = Skeleton.content SessionMsg SessionMsg <| Home.view model.user
                    }

                Router.Dashboard ->
                    { title = Dashboard.title
                    , body = Skeleton.application ProjectMsg SessionMsg SkeletonMsg model.sidebar <| Dashboard.view model.user model.projects
                    }

                Router.CreateProject ->
                    { title = Project.createProjectTitle
                    , body = Skeleton.application ProjectMsg SessionMsg SkeletonMsg model.sidebar <| Project.createProjectView model.user model.projects
                    }

                Router.EditProject uuid ->
                    { title = Project.editProjectTitle
                    , body = Skeleton.application ProjectMsg SessionMsg SkeletonMsg model.sidebar <| Project.editProjectView model.user model.projects
                    }

                _ ->
                    { title = NotFound.title
                    , body = Skeleton.content NotFoundMsg SessionMsg NotFound.view
                    }
    in
    { title = document.title, body = [ Html.map Forward <| document.body ] }



-- UPDATE
-- Proxy Messages - Forwarding message to their respective modules


type ProxyMsg
    = SessionMsg Session.Msg
    | ProofMsg Proof.Msg
    | ProjectMsg Project.Msg
    | SkeletonMsg Skeleton.Msg
    | NotFoundMsg NotFound.Msg


proxyMsg : ProxyMsg -> Model -> ( Model, Cmd Msg )
proxyMsg msg model =
    case msg of
        SessionMsg sessionMsg ->
            let
                ( sessionUser, sessionCmds ) =
                    Session.update sessionMsg ( model.user, model.key )
            in
            ( { model | user = sessionUser }, Cmd.map Forward <| Cmd.map SessionMsg sessionCmds )

        ProjectMsg projectMsg ->
            let
                ( newProjects, projectCmds ) =
                    Project.update projectMsg model.projects model.key
            in
            ( { model | projects = newProjects }, Cmd.map Forward <| Cmd.map ProjectMsg projectCmds )

        SkeletonMsg skeletonMsg ->
            ( { model | sidebar = Skeleton.update model.sidebar skeletonMsg }, Cmd.none )

        _ ->
            ( model, Cmd.none )


type Msg
    = LinkClicked UrlRequest
    | Forward ProxyMsg
    | UrlChanged Url


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Forward msg ->
            proxyMsg msg model

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        page =
                            Router.route url
                    in
                    ( { model | page = page }
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            let
                page =
                    Router.route url
            in
            ( { model | page = page }, Cmd.none )
