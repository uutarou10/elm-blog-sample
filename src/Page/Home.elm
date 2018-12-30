module Page.Home exposing (Model, Msg(..), init, update, view)

import Article
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode


type alias Model =
    { articles : ArticlesStatus }


type ArticlesStatus
    = Loading
    | Fail Http.Error
    | Success (List Article.Article)


init : ( Model, Cmd Msg )
init =
    ( Model Loading, getArticles )


getArticles : Cmd Msg
getArticles =
    Http.get
        { url = "http://localhost:8080/posts"
        , expect = Http.expectJson GotArticles (Decode.list Article.decoder)
        }


type Msg
    = NoOp
    | GotArticles (Result Http.Error (List Article.Article))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotArticles response ->
            case response of
                Ok articles ->
                    ( { model | articles = Success articles }, Cmd.none )

                Err err ->
                    ( { model | articles = Fail err }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        viewArticle : Article.Article -> Html Msg
        viewArticle article =
            div []
                [ h2 [] [ a [ href <| "/articles/" ++ String.fromInt article.id ] [ text article.title ] ]
                , p [] [ text <| String.left 100 article.body ++ "..." ]
                ]

        viewArticles : List Article.Article -> List (Html Msg)
        viewArticles articles =
            List.map viewArticle articles
    in
    div []
        [ h1 [] [ text "Elm blog sample" ]
        , div []
            (case model.articles of
                Loading ->
                    [ p [] [ text "Loading..." ] ]

                Fail _ ->
                    [ p [] [ text "Error" ] ]

                Success articles ->
                    viewArticles articles
            )
        ]
