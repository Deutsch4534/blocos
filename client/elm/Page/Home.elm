module Page.Home exposing (route, title, view)

import Html exposing (Html, article, button, form, h1, h2, input, p, section, text)
import Html.Attributes exposing (action, class, id, method, name, placeholder, type_, value)
import Html.Events exposing (onClick)
import Session


route : String
route =
    "/"


title : String
title =
    "Blocos - Creative project funding powered by Bitcoin"


view : Session.User -> Html Session.Msg
view user =
    let
        username =
            case user of
                ( Session.LoggedIn, Just userData ) ->
                    userData.username

                _ ->
                    "Anonymous"
    in
    section [ class "home" ]
        [ section [ class "banner" ]
            [ h1 [ class "banner-title" ] [ text "Blockchain powered crowdfunding" ]
            , p [ class "banner-subtitle" ] [ text "Blocos is a new kind of crowdfunding platform built on top of new internet technology. Because of the way it is built, Blocos adds key features to crowdfunding as we know it." ]
            ]
        , section [ class "features" ]
            [ article [ class "feature" ]
                [ h2 [ class "feature-title" ] [ text "Global audience" ]
                , p [ class "feature-description" ] [ text "Bitcoin has no boarders. By creating your project on Blocos, you’re ready to get funded by people from all over the world." ]
                ]
            , article [ class "feature" ]
                [ h2 [ class "feature-title" ] [ text "Creative freedom" ]
                , p [ class "feature-description" ] [ text "With Blocos, you’re in control of your data. For project creators, this means total creative freedom over what you decide to fund." ]
                ]
            , article [ class "feature" ]
                [ h2 [ class "feature-title" ] [ text "Transparency" ]
                , p [ class "feature-description" ] [ text "All transactions are written to a public ledger - a blockchain. Anyone can audit and verify every project on the platform." ]
                ]
            ]
        , section [ id "newsletter", class "newsletter" ]
            [ p [ class "newsletter-info" ] [ text "Join our newsletter to know more about the platform and when it will be released." ]
            , form [ class "newsletter-form", action "https://gmail.us20.list-manage.com/subscribe/post?u=541e810c6ff7316d44044e4c5&amp;id=7fb8b97eb9", method "POST" ]
                [ input [ class "newsletter-input", placeholder "crypto@bitcoin.com", type_ "email", name "EMAIL" ] []
                , input [ class "hidden-visible-input", name "b_541e810c6ff7316d44044e4c5_7fb8b97eb9", type_ "text", value "" ] []
                , input [ class "newsletter-submit", type_ "submit", value "join newsletter" ] []
                ]
            ]
        ]
