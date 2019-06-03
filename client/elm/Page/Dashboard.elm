module Page.Dashboard exposing (projectRoot, route, title, view)

import Browser
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Encode as Encode exposing (..)
import List
import Project
import Session
import Url.Builder


route : String
route =
    "dashboard"


projectRoot : String
projectRoot =
    "projects"


title : String
title =
    "Blocos app"


view : Session.User -> Project.Model -> Html.Html Project.Msg
view _ ( _, projects, _ ) =
    let
        renderCreatedProjects =
            if List.length projects == 0 then
                Html.article
                    [ Attributes.class "feature-banner" ]
                    [ Html.h1 [ Attributes.class "feature-banner__title" ] [ Html.text "Time to make it happen" ]
                    , Html.h2 [ Attributes.class "feature-banner__subtitle" ] [ Html.text "Get your community together and start your next big thing" ]
                    , Html.a [ Attributes.class "submit submit-link", Attributes.href <| Project.createProjectRoute ] [ Html.text "start new project" ]
                    , Html.p [ Attributes.class "feature-banner__disclaimer" ] [ Html.text "powered by Bitcoin" ]
                    ]

            else
                Html.article
                    [ Attributes.class "projects created-projects" ]
                    [ Html.h1 [ Attributes.class "dashboard-title" ] [ Html.text "Created projects" ]
                    , Html.ul [ Attributes.class "projects-list" ]
                        (List.map
                            (\projectItem ->
                                Html.li [ Attributes.class "projecs-list projects-list__item" ]
                                    [ Html.a
                                        [ Attributes.class "project-link link"
                                        , Attributes.href (Project.getEditProjectRoute projectItem)
                                        , Events.onClick <| Project.EditProject projectItem
                                        ]
                                        [ Html.text projectItem.title ]
                                    ]
                            )
                            projects
                        )
                    ]

        renderBackedProjects =
            Html.div [ Attributes.class "projects backed-projects" ] []
    in
    Html.section [ Attributes.class "dashboard" ]
        [ renderCreatedProjects
        , renderBackedProjects
        ]
