module Page.Projects exposing (Data, Model, Msg, page)

import Api.Project exposing (Project)
import DataSource exposing (DataSource)
import Element exposing (..)
import Element.Background as Background
import Head
import Head.Seo as Seo
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


type alias Data =
    List Project


data : DataSource Data
data =
    Api.Project.allProjects


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = "Projects"
    , body =
        [ column
            [ height fill
            , centerX
            , padding 30
            , spacing 20
            ]
            [ el [ centerX, alignTop ] <| text "Projects"
            , el [ alignTop ]
                (text ("Anzahl: " ++ (static.data |> List.length |> String.fromInt)))
            , column
                [ centerX
                , height fill
                , scrollbarY
                ]
                (static.data |> List.map viewProject)
            ]
        ]
    }


viewProject : Project -> Element msg
viewProject project =
    column
        []
        [ el [] <| text project.title
        , el
            [ width <| px 400
            , height <| px 400
            , Background.image project.imageUrl
            ]
            none
        , case project.description of
            Just description ->
                text description

            Nothing ->
                none
        ]
