module Router exposing (Page(..), route)

import Page.Dashboard as Dashboard
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Proof as Proof
import Url
import Url.Parser as Parser exposing ((</>))


type Page
    = Home
    | NotFound
    | Proof
    | Dashboard
    | CreateProject
    | EditProjectReward String
    | EditProject String


routeParser : Parser.Parser (Page -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Proof (Parser.s Proof.route)
        , Parser.map Dashboard (Parser.s Dashboard.route)
        , Parser.map CreateProject (Parser.s "projects" </> Parser.s "new")
        , Parser.map EditProjectReward (Parser.s "projects" </> Parser.s "edit" </> Parser.string </> Parser.s "rewards")
        , Parser.map EditProject (Parser.s "projects" </> Parser.s "edit" </> Parser.string)
        ]


route : Url.Url -> Page
route url =
    Maybe.withDefault NotFound (Parser.parse routeParser url)
