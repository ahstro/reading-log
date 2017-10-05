module Json
    exposing
        ( decodeISBN
        , decodeBookFromOpenLibrary
        , booksDecoder
        , pageDecoder
        , encodeModel
        )

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


decodeBookFromOpenLibrary : String -> Decode.Decoder Book
decodeBookFromOpenLibrary isbnString =
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


isbnDecoder : Decode.Decoder ISBN
isbnDecoder =
    Decode.int
        |> Decode.andThen (\isbn -> Decode.succeed (ISBN isbn))


bookDecoder : Decode.Decoder Book
bookDecoder =
    (Decode.map4 Book
        (Decode.field "name" Decode.string)
        (Decode.field "by" Decode.string)
        (Decode.field "pageCount" pageDecoder)
        (Decode.field "isbn" isbnDecoder)
    )


booksDecoder : Decode.Decoder (EveryDict ISBN Book)
booksDecoder =
    (Decode.keyValuePairs Decode.value)
        |> Decode.andThen decodeBooksListToEveryDict


decodeBooksListToEveryDict : List ( String, Decode.Value ) -> Decode.Decoder (EveryDict ISBN Book)
decodeBooksListToEveryDict =
    List.filterMap stringValueToMaybeISBNBook
        >> EveryDict.fromList
        >> Decode.succeed


stringValueToMaybeISBNBook : ( String, Decode.Value ) -> Maybe ( ISBN, Book )
stringValueToMaybeISBNBook ( isbnString, bookValue ) =
    case
        ( Decode.decodeString isbnDecoder isbnString
        , Decode.decodeValue bookDecoder bookValue
        )
    of
        ( Ok isbn, Ok book ) ->
            Just ( isbn, book )

        ( Err isbnErr, Err bookErr ) ->
            debugAndReturn Nothing "Error decoding isbn and book:" ( isbnErr, bookErr )

        ( Err isbnErr, _ ) ->
            debugAndReturn Nothing "Error decoding isbn:" isbnErr

        ( _, Err bookErr ) ->
            debugAndReturn Nothing "Error decoding book:" bookErr


debugAndReturn : a -> String -> b -> a
debugAndReturn a errString err =
    let
        _ =
            Debug.log errString err
    in
        a
