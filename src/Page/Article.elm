module Page.Article exposing (ArticleStatus(..), Model, Msg(..), getArticle, init, update, view)

import Article
import Html exposing (..)
import Http


init : String -> ( Model, Cmd Msg )
init id =
    ( Model Loading, getArticle id )


type alias Model =
    { article : ArticleStatus }


type ArticleStatus
    = Loading
    | Fail Http.Error
    | Success Article.Article


type Msg
    = NoOp
    | GotArticle (Result Http.Error Article.Article)


getArticle : String -> Cmd Msg
getArticle id =
    Http.get
        { url = "http://localhost:8080/posts/" ++ id
        , expect = Http.expectJson GotArticle Article.decoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotArticle result ->
            case result of
                Ok article ->
                    ( { model | article = Success article }, Cmd.none )

                Err err ->
                    ( { model | article = Fail err }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm blog sample" ]
        , case model.article of
            Loading ->
                p [] [ text "Loading..." ]

            Fail err ->
                p [] [ text "Error" ]

            Success article ->
                Html.article []
                    [ h2 [] [ text article.title ]
                    , div [] [ text article.body ]
                    ]
        ]
