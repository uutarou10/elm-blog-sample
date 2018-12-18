module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Url
import Url.Parser as Parser



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , currentRoute : Route
    , articles : List Article
    }


type Route
    = Top
    | NotFound


type alias Article =
    { id : Int
    , title : String
    , body : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        currentRoute =
            Parser.parse parser url
    in
    ( { key = key
      , currentRoute = parse url
      , articles = []
      }
    , getArticles
    )



---- UPDATE ----


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External urlStr ->
                    ( model, Nav.load urlStr )



---- URL PARSER ---


parser : Parser.Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Top <| Parser.top ]


parse : Url.Url -> Route
parse url =
    Maybe.withDefault NotFound <| Parser.parse parser url



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.currentRoute of
        Top ->
            { title = "title"
            , body =
                [ p [] [ text "hello" ]
                ]
            }

        NotFound ->
            viewNotFound


viewNotFound : Browser.Document Msg
viewNotFound =
    { title = "Not found"
    , body =
        [ h1 [] [ text "Not found..." ] ]
    }



---- HTTP ----


articleDecoder : Decode.Decoder Article
articleDecoder =
    Decode.map3 Article
        (Decode.field "id" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "body" Decode.string)


articlesDecoder : Decode.Decoder (List Article)
articlesDecoder =
    Decode.list articleDecoder


getArticles : Cmd Msg
getArticles =
    Http.get
        { url = "http://localhost:8080/posts"
        , expect = Http.expectJson GotArticles articlesDecoder
        }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
