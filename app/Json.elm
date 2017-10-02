module Json exposing (decodeISBN, decodeBook, pageDecoder, encodeModel)

import Model
    exposing
        ( Model
        , Book
        , ISBN(..)
        , Page(..)
        , addBook
        )
import Json.Decode as Decode
import Json.Encode as Encode
import EveryDict exposing (EveryDict)


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "books", encodeBooks model.books )
        , ( "progress", encodeProgress model.progress )
        ]


encodeProgress : EveryDict ISBN Page -> Encode.Value
encodeProgress progress =
    progress
        |> EveryDict.toList
        |> List.map
            (\( ISBN isbn, Page page ) ->
                ( toString isbn
                , Encode.int page
                )
            )
        |> Encode.object


encodeBooks : EveryDict ISBN Book -> Encode.Value
encodeBooks books =
    books
        |> EveryDict.toList
        |> List.map
            (\( ISBN isbn, book ) ->
                ( toString isbn
                , encodeBook book
                )
            )
        |> Encode.object


encodeBook : Book -> Encode.Value
encodeBook book =
    Encode.object
        [ ( "name", Encode.string book.name )
        , ( "by", Encode.string book.by )
        , ( "pageCount", encodePage book.pageCount )
        , ( "isbn", encodeISBN book.isbn )
        ]


encodeISBN : ISBN -> Encode.Value
encodeISBN (ISBN isbn) =
    Encode.int isbn


encodePage : Page -> Encode.Value
encodePage (Page page) =
    Encode.int page


decodeBook : String -> Decode.Decoder Book
decodeBook isbnString =
    (Decode.field ("ISBN:" ++ isbnString)
        (Decode.map4 Book
            (Decode.field "title" Decode.string)
            (Decode.field "by_statement" Decode.string)
            (Decode.field "number_of_pages" pageDecoder)
            (decodeISBN isbnString)
        )
    )


pageDecoder : Decode.Decoder Page
pageDecoder =
    Decode.int
        |> Decode.andThen (Decode.succeed << Page)


decodeISBN : String -> Decode.Decoder ISBN
decodeISBN isbnString =
    case String.toInt isbnString of
        Ok isbn ->
            Decode.succeed (ISBN isbn)

        Err err ->
            Decode.fail err
