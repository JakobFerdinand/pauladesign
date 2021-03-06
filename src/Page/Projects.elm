module Page.Projects exposing (Data, Model, Msg, page)

import Api.Project exposing (Project)
import DataSource exposing (DataSource)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Head
import Head.Seo as Seo
import Markdown exposing (TableOfContent, markdown)
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
    link []
        { url = "/projects/" ++ project.slug
        , label =
            column
                []
                [ el [ Font.bold ] <| text project.title
                , el
                    [ width <| px 400
                    , height <| px 400
                    , Background.image project.mainImageUrl
                    ]
                    none
                , case project.projectDescription of
                    Just description ->
                        case markdown <| String.replace "\u{000D}" "" description of
                            Ok ( toc, renderedEls ) ->
                                column
                                    [ spacing 30
                                    , padding 10
                                    , centerX
                                    , width fill
                                    ]
                                    renderedEls

                            Err errors ->
                                paragraph []
                                    [ text "I??m sorry but it looks like I published invalid markdown."
                                    , text "Feel free to contact me so I can updated and fix the problem."
                                    ]

                    Nothing ->
                        none
                ]
        }
