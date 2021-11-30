module Page.Projects.Project_ exposing (Data, Model, Msg, page)

import Api.Project
import DataSource exposing (DataSource)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Head
import Head.Seo as Seo
import Markdown exposing (markdown)
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
    { project : String }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , routes = routes
        , data = data
        }
        |> Page.buildNoState { view = view }


routes : DataSource (List RouteParams)
routes =
    Api.Project.allProjects
        |> DataSource.map (List.map (\project -> { project = project.slug }))


data : RouteParams -> DataSource Data
data routeParams =
    Api.Project.projectDetails routeParams.project


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


type alias Data =
    Api.Project.ProjectDetails


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = static.data.title
    , body =
        [ el [ scrollbarY, height fill, width fill ] <|
            column
                [ centerX
                , height fill
                , spacing 20
                ]
                [ el [ Font.bold, centerX ] <| text static.data.title
                , el
                    [ height <| px 400
                    , width <| px 400
                    , Background.image static.data.mainImageUrl
                    ]
                    none
                , case static.data.projectDescription of
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
                                    [ text "I´m sorry but it looks like I published invalid markdown."
                                    , text "Feel free to contact me so I can updated and fix the problem."
                                    ]

                    Nothing ->
                        none
                , row [ spacing 10 ]
                    (static.data.imageUrls |> List.map (\url -> el [ width <| px 250, height <| px 250, Background.image url ] none))
                , case static.data.enhancedProjectDescription of
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
                                    [ text "I´m sorry but it looks like I published invalid markdown."
                                    , text "Feel free to contact me so I can updated and fix the problem."
                                    ]

                    Nothing ->
                        none
                ]
        ]
    }
