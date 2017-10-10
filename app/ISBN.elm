module ISBN
    exposing
        ( ISBN(..)
        , get
        , isbnDecoder
        , decodeISBN
        , encodeISBN
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


-- Types


type ISBN
    = ISBN Int



-- Helpers


get : ISBN -> Int
get (ISBN isbn) =
    isbn



-- JSON decoders


isbnDecoder : Decoder ISBN
isbnDecoder =
    Decode.int
        |> Decode.andThen (\isbn -> Decode.succeed (ISBN isbn))


decodeISBN : String -> Decoder ISBN
decodeISBN isbnString =
    case String.toInt isbnString of
        Ok isbn ->
            Decode.succeed (ISBN isbn)

        Err err ->
            Decode.fail err



-- JSON encoders


encodeISBN : ISBN -> Encode.Value
encodeISBN (ISBN isbn) =
    Encode.int isbn
