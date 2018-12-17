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
    , activities : List Activity
    }


type Route
    = Top
    | NotFound


type alias Activity =
    { title : String
    , media : String
    , url : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        currentRoute =
            Parser.parse parser url
    in
    ( { key = key
      , currentRoute = parse url
      , activities = []
      }
    , Cmd.none
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


type alias Data =
    { title : String
    , body : String
    }


viewDataList : List Data -> Html Msg
viewDataList dataList =
    let
        transform : Data -> List (Html Msg)
        transform data =
            [ dt [] [ text data.title ]
            , dd [] [ text data.body ]
            ]
    in
    dl [] <| List.concatMap transform dataList


viewActivityList : List Activity -> Html Msg
viewActivityList list =
    let
        transform : Activity -> Html Msg
        transform activity =
            li [] [ a [ href activity.url ] [ text <| activity.title ++ "(" ++ activity.media ++ ")" ] ]
    in
    if List.length list == 0 then
        p [] [ text "No content..." ]

    else
        ul [] <| List.map transform list



---- HTTP ----
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
