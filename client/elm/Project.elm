module Project exposing
    ( Model
    , Msg(..)
    , Project
    , ProjectStatus(..)
    , createProjectRoute
    , createProjectTitle
    , createProjectView
    , editProjectTitle
    , editProjectView
    , emptyProject
    , getEditProjectRoute
    , setCurrentProjectByUuidString
    , subscriptions
    , update
    )

import Browser
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Encode as Encode
import List
import Port.Blockstack as Blockstack
import Prng.Uuid as Uuid
import Random.Pcg.Extended as Random
import Session
import String
import Url.Builder


type ProjectStatus
    = Unsaved
    | Saving
    | Saved
    | Publishing
    | Published


type alias WalletAddress =
    String


type alias Project =
    { uuid : Maybe Uuid.Uuid
    , address : Maybe WalletAddress
    , description : String
    , featuredImageUrl : String
    , goal : Float
    , status : ProjectStatus
    , title : String
    }


type alias Model =
    ( Project, List Project, Random.Seed )


emptyProject : Project
emptyProject =
    { uuid = Nothing
    , address = Nothing
    , description = ""
    , featuredImageUrl = ""
    , goal = 0.0
    , status = Saved
    , title = ""
    }


type Msg
    = DeleteProject
    | ProjectDeleted
    | SaveProject
    | ProjectSaved Blockstack.ProjectFile
    | ChangeTitle String
    | ChangeDescription String
    | ChangeGoal String
    | EditProject Project


parseProjectToFile : Project -> Blockstack.ProjectFile
parseProjectToFile project =
    let
        uuidString =
            case project.uuid of
                Just uuid ->
                    Uuid.toString uuid

                Nothing ->
                    ""
    in
    { address = project.address
    , description = project.description
    , featuredImageUrl = project.featuredImageUrl
    , goal = project.goal
    , title = project.title
    , uuid = uuidString
    }


parseFileToProject : Blockstack.ProjectFile -> Project
parseFileToProject projectFile =
    { uuid = Uuid.fromString projectFile.uuid
    , address = projectFile.address
    , description = projectFile.description
    , featuredImageUrl = projectFile.featuredImageUrl
    , status = Saved
    , goal = projectFile.goal
    , title = projectFile.title
    }


updateIfProject : Project -> Project -> Project
updateIfProject savingProject projectOnList =
    if savingProject.uuid == projectOnList.uuid then
        savingProject

    else
        projectOnList


hasProject : List Project -> Project -> Bool
hasProject projects project =
    List.any (\currentProject -> currentProject.uuid == project.uuid) projects


reconcileProjects : List Project -> Project -> List Project
reconcileProjects projects project =
    if hasProject projects project then
        List.map (updateIfProject project) projects

    else
        project :: projects


removeProject : List Project -> Project -> List Project
removeProject projects project =
    List.filter (\item -> item.uuid /= project.uuid) projects


getEditProjectRoute : Project -> String
getEditProjectRoute project =
    case project.uuid of
        Just uuid ->
            Url.Builder.absolute [ "projects", "edit", Uuid.toString uuid ] []

        Nothing ->
            Url.Builder.absolute [ "projects", "new" ] []


setCurrentProjectByUuidString : String -> Model -> Model
setCurrentProjectByUuidString uuidString ( _, projects, seed ) =
    let
        parsedUuid =
            Uuid.fromString uuidString

        getProject projectUuid =
            Maybe.withDefault emptyProject (List.head <| List.filter (\project -> project.uuid == projectUuid) projects)
    in
    case parsedUuid of
        Just uuid ->
            ( getProject parsedUuid, projects, seed )

        Nothing ->
            ( emptyProject, projects, seed )


setUuidIfEmpty : Project -> Random.Seed -> ( Project, Random.Seed )
setUuidIfEmpty project seed =
    case project.uuid of
        Just uuid ->
            ( project, seed )

        Nothing ->
            let
                ( newUuid, newSeed ) =
                    Random.step Uuid.generator seed
            in
            ( { project | uuid = Just newUuid }, newSeed )


redirectToProjectList : Nav.Key -> Cmd Msg
redirectToProjectList navKey =
    Nav.pushUrl navKey (Url.Builder.absolute [ "dashboard" ] [])


redirectToEditPage : Project -> Nav.Key -> Cmd Msg
redirectToEditPage project navKey =
    Nav.pushUrl navKey (getEditProjectRoute project)


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Blockstack.fileSaved (\value -> ProjectSaved value)
        , Blockstack.fileDeleted (\_ -> ProjectDeleted)
        ]


update : Msg -> Model -> Nav.Key -> ( Model, Cmd Msg )
update msg ( project, projects, seed ) navKey =
    case msg of
        DeleteProject ->
            let
                updatedProjects =
                    removeProject projects project
            in
            ( ( project, updatedProjects, seed ), Blockstack.deleteFile (parseProjectToFile project) )

        SaveProject ->
            let
                ( projectToSave, newSeed ) =
                    setUuidIfEmpty project seed
            in
            ( ( { projectToSave | status = Saving }, projects, newSeed ), Blockstack.putFile (parseProjectToFile projectToSave) )

        ProjectSaved savedProjectFile ->
            let
                savedProject =
                    parseFileToProject savedProjectFile

                updatedProjects =
                    reconcileProjects projects savedProject
            in
            ( ( emptyProject, updatedProjects, seed ), redirectToProjectList navKey )

        ProjectDeleted ->
            ( ( emptyProject, projects, seed ), redirectToProjectList navKey )

        EditProject projectToEdit ->
            ( ( projectToEdit, projects, seed ), Cmd.none )

        ChangeDescription newDescription ->
            ( ( { project | description = newDescription, status = Unsaved }, projects, seed ), Cmd.none )

        ChangeTitle newTitle ->
            ( ( { project | title = newTitle, status = Unsaved }, projects, seed ), Cmd.none )

        ChangeGoal maybeNewGoal ->
            let
                newGoal =
                    case String.toFloat maybeNewGoal of
                        Just goal ->
                            goal

                        Nothing ->
                            0.0
            in
            ( ( { project | goal = newGoal, status = Unsaved }, projects, seed ), Cmd.none )


createProjectRoute : String
createProjectRoute =
    Url.Builder.absolute [ "projects", "new" ] []


createProjectTitle : String
createProjectTitle =
    "Create your new descentralized crowdfunding project - Blocos"


editProjectTitle : String
editProjectTitle =
    "Edit project - Blocos"


buttonLabel : Bool -> String
buttonLabel isSaving =
    if isSaving == True then
        "Saving..."

    else
        "Save"


createProjectView : Session.User -> Model -> Html.Html Msg
createProjectView user ( currentProject, _, _ ) =
    let
        username =
            case user of
                ( Session.LoggedIn, Just userData ) ->
                    userData.username

                _ ->
                    "Anonymous"

        isSaving =
            case currentProject.status of
                Saving ->
                    True

                _ ->
                    False
    in
    Html.section [ Attributes.class "create-project" ]
        [ Html.h1 [ Attributes.class "title" ] [ Html.text "Create your new project" ]
        , Html.form
            [ Attributes.class "form form-project"
            , Attributes.name "project"
            , Attributes.action "#"
            , Events.onSubmit SaveProject
            ]
            [ Html.fieldset
                [ Attributes.class "fieldset" ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.for "project-title"
                    ]
                    [ Html.text "Title" ]
                , Html.input
                    [ Attributes.id "project-title"
                    , Attributes.class "input"
                    , Attributes.type_ "text"
                    , Attributes.value currentProject.title
                    , Attributes.placeholder "Project title"
                    , Events.onInput ChangeTitle
                    ]
                    []
                ]
            , Html.fieldset
                [ Attributes.class "fieldset" ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.for "project-goal"
                    ]
                    [ Html.text "Goal (in btc)" ]
                , Html.input
                    [ Attributes.class "input"
                    , Attributes.for "project-goal"
                    , Attributes.type_ "number"
                    , Attributes.step ".0000001"
                    , Attributes.value <| String.fromFloat currentProject.goal
                    , Attributes.placeholder "0.0003"
                    , Events.onInput ChangeGoal
                    ]
                    []
                ]
            , Html.fieldset
                [ Attributes.class "fieldset" ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.for "project-description"
                    ]
                    [ Html.text "Description" ]
                , Html.textarea
                    [ Attributes.class "input textarea"
                    , Attributes.name "project-description"
                    , Attributes.placeholder "Let's fix the world, 1 btc at a time"
                    , Events.onInput ChangeDescription
                    ]
                    [ Html.text currentProject.description ]
                ]
            , Html.input
                [ Attributes.class "submit button -reverse"
                , Attributes.type_ "submit"
                , Attributes.disabled isSaving
                , Attributes.value <| buttonLabel isSaving
                ]
                []
            ]
        ]


editProjectView : Session.User -> Model -> Html.Html Msg
editProjectView user ( currentProject, projects, seed ) =
    let
        username =
            case user of
                ( Session.LoggedIn, Just userData ) ->
                    userData.username

                _ ->
                    "Anonymous"

        isSaving =
            case currentProject.status of
                Saving ->
                    True

                _ ->
                    False
    in
    Html.section [ Attributes.class "create-project" ]
        [ Html.h1 [ Attributes.class "title" ] [ Html.text "Edit project" ]
        , Html.form
            [ Attributes.class "form form-project"
            , Attributes.name "project"
            , Attributes.action "#"
            , Events.onSubmit SaveProject
            ]
            [ Html.fieldset
                [ Attributes.class "fieldset" ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.for "project-title"
                    ]
                    [ Html.text "Title" ]
                , Html.input
                    [ Attributes.id "project-title"
                    , Attributes.class "input"
                    , Attributes.type_ "text"
                    , Attributes.value currentProject.title
                    , Attributes.placeholder "Project title"
                    , Events.onInput ChangeTitle
                    ]
                    []
                ]
            , Html.fieldset
                [ Attributes.class "fieldset" ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.for "project-goal"
                    ]
                    [ Html.text "Goal (in btc)" ]
                , Html.input
                    [ Attributes.class "input"
                    , Attributes.for "project-goal"
                    , Attributes.type_ "number"
                    , Attributes.step ".0000001"
                    , Attributes.value <| String.fromFloat currentProject.goal
                    , Attributes.placeholder "0.0003"
                    , Events.onInput ChangeGoal
                    ]
                    []
                ]
            , Html.fieldset
                [ Attributes.class "fieldset" ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.for "project-description"
                    ]
                    [ Html.text "Description" ]
                , Html.textarea
                    [ Attributes.class "input textarea"
                    , Attributes.name "project-description"
                    , Attributes.placeholder "Let's fix the world, 1 btc at a time"
                    , Events.onInput ChangeDescription
                    ]
                    [ Html.text currentProject.description ]
                ]
            , Html.input
                [ Attributes.class "submit button -reverse"
                , Attributes.type_ "submit"
                , Attributes.disabled isSaving
                , Attributes.value <| buttonLabel isSaving
                ]
                []
            , Html.input
                [ Attributes.class "submit button"
                , Attributes.type_ "button"
                , Attributes.value "Delete"
                , Events.onClick DeleteProject
                ]
                []
            ]
        ]
