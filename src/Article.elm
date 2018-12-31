module Article exposing (Article, decoder)

import ISO8601
import Json.Decode as Decode
import Time


type alias Article =
    { id : Int
    , title : String
    , body : String
    , createdAt : Date
    }


type alias Date =
    { zone : Time.Zone
    , time : Time.Posix
    }


decoder : Decode.Decoder Article
decoder =
    Decode.map4 Article
        (Decode.field "id" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "body" Decode.string)
        (Decode.field "created_at" dateDecoder)


dateDecoder : Decode.Decoder Date
dateDecoder =
    let
        convert : String -> Decode.Decoder Date
        convert raw =
            case ISO8601.fromString raw of
                Ok time ->
                    let
                        ( offsetHours, offsetMinutes ) =
                            time.offset
                    in
                    Decode.succeed <|
                        Date
                            (Time.customZone ((offsetHours * 60) + offsetMinutes) [])
                            (ISO8601.toPosix time)

                Err err ->
                    Decode.fail err
    in
    Decode.string |> Decode.andThen convert
