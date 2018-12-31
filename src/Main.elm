module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Page.Article as Article
import Page.Home as Home
import Url
import Url.Parser as Parser exposing ((</>))


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    stepUrl url
        { key = key
        , page = NotFound
        }


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | Home Home.Model
    | Article Article.Model



---- UPDATE ----


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | ArticleMsg Article.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            stepUrl url model

        HomeMsg msg ->
            case model.page of
                Home home ->
                    stepHome model (Home.update msg home)

                _ ->
                    ( model, Cmd.none )

        ArticleMsg msg ->
            case model.page of
                Article article ->
                    stepArticle model (Article.update msg article)

                _ ->
                    ( model, Cmd.none )


stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
stepHome model ( homeModel, homeMsg ) =
    ( { model | page = Home homeModel }
    , Cmd.map HomeMsg homeMsg
    )


stepArticle : Model -> ( Article.Model, Cmd Article.Msg ) -> ( Model, Cmd Msg )
stepArticle model ( articleModel, articleMsg ) =
    ( { model | page = Article articleModel }
    , Cmd.map ArticleMsg articleMsg
    )


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            Parser.oneOf
                [ Parser.map (stepHome model Home.init) Parser.top
                , Parser.map (\articleId -> stepArticle model (Article.init articleId)) (Parser.s "articles" </> Parser.string)
                ]
    in
    case Parser.parse parser url of
        Just ans ->
            ans

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            { title = "Not Found"
            , body = [ text "Not Found" ]
            }

        Home home ->
            { title = "Home | Elm blog sample"
            , body = [ Html.map HomeMsg (Home.view home) ]
            }

        Article article ->
            let
                title =
                    case article.article of
                        Article.Success a ->
                            a.title

                        _ ->
                            "Article"
            in
            { title = title ++ " | Elm blog sample"
            , body = [ Html.map ArticleMsg (Article.view article) ]
            }
