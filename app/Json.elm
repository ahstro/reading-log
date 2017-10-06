module Json
    exposing
        ( decodeISBN
        , decodeBookFromOpenLibrary
        , progressDecoder
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
        )
import Json.Decode as Decode exposing (Decoder)
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


decodeBookFromOpenLibrary : String -> Decoder Book
decodeBookFromOpenLibrary isbnString =
    (Decode.field ("ISBN:" ++ isbnString)
        (Decode.map4 Book
            (Decode.field "title" Decode.string)
            (Decode.field "by_statement" Decode.string)
            (Decode.field "number_of_pages" pageDecoder)
            (decodeISBN isbnString)
        )
    )


pageDecoder : Decoder Page
pageDecoder =
    Decode.int
        |> Decode.andThen (Decode.succeed << Page)


decodeISBN : String -> Decoder ISBN
decodeISBN isbnString =
    case String.toInt isbnString of
        Ok isbn ->
            Decode.succeed (ISBN isbn)

        Err err ->
            Decode.fail err


isbnDecoder : Decoder ISBN
isbnDecoder =
    Decode.int
        |> Decode.andThen (\isbn -> Decode.succeed (ISBN isbn))


bookDecoder : Decoder Book
bookDecoder =
    (Decode.map4 Book
        (Decode.field "name" Decode.string)
        (Decode.field "by" Decode.string)
        (Decode.field "pageCount" pageDecoder)
        (Decode.field "isbn" isbnDecoder)
    )


booksDecoder : Decoder (EveryDict ISBN Book)
booksDecoder =
    (Decode.keyValuePairs Decode.value)
        |> Decode.andThen
            (decodeKeyValuePairsToEveryDict ( isbnDecoder, bookDecoder ))


decodeKeyValuePairsToEveryDict :
    ( Decoder a, Decoder b )
    -> List ( String, Decode.Value )
    -> Decoder (EveryDict a b)
decodeKeyValuePairsToEveryDict ( aDecoder, bDecoder ) =
    List.filterMap (decodePairToMaybe ( aDecoder, bDecoder ))
        >> EveryDict.fromList
        >> Decode.succeed


decodePairToMaybe :
    ( Decoder a, Decoder b )
    -> ( String, Decode.Value )
    -> Maybe ( a, b )
decodePairToMaybe ( aDecoder, bDecoder ) ( aString, bValue ) =
    case
        ( Decode.decodeString aDecoder aString
        , Decode.decodeValue bDecoder bValue
        )
    of
        ( Ok a, Ok b ) ->
            Just ( a, b )

        ( Err aErr, Err bErr ) ->
            debugAndReturn Nothing "Error decoding a and b:" ( aErr, bErr )

        ( Err aErr, _ ) ->
            debugAndReturn Nothing "Error decoding a:" aErr

        ( _, Err bErr ) ->
            debugAndReturn Nothing "Error decoding b:" bErr


progressDecoder : Decoder (EveryDict ISBN Page)
progressDecoder =
    (Decode.keyValuePairs Decode.value)
        |> Decode.andThen
            (decodeKeyValuePairsToEveryDict ( isbnDecoder, pageDecoder ))


debugAndReturn : a -> String -> b -> a
debugAndReturn a errString err =
    let
        _ =
            Debug.log errString err
    in
        a
