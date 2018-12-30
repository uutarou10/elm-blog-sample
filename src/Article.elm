module Article exposing (Article, decoder)

import Json.Decode as Decode


type alias Article =
    { id : Int
    , title : String
    , body : String
    }


decoder : Decode.Decoder Article
decoder =
    Decode.map3 Article
        (Decode.field "id" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "body" Decode.string)
