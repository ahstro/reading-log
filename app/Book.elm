module Book
    exposing
        ( Book
        , Books
        , decodeBookFromOpenLibrary
        , booksDecoder
        , encodeBooks
        , addBook
        )

import ISBN exposing (ISBN(..), isbnDecoder, decodeISBN, encodeISBN)
import Page exposing (Page, decodePage, pageDecoder, encodePage)
import EveryDict exposing (EveryDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


-- Types


type alias Book =
    { name : String
    , by : String
    , progress : Page
    , pageCount : Page
    , isbn : ISBN
    }


type alias Books =
    EveryDict ISBN Book


type alias HasBooks r =
    { r | books : Books }



-- Helper functions


addBook : Book -> HasBooks r -> HasBooks r
addBook book r =
    { r
        | books =
            EveryDict.insert
                book.isbn
                book
                r.books
    }



-- JSON decoders


decodeBookFromOpenLibrary : String -> Decoder Book
decodeBookFromOpenLibrary isbnString =
    (Decode.field ("ISBN:" ++ isbnString)
        (Decode.map5 Book
            (Decode.field "title" Decode.string)
            (Decode.field "by_statement" Decode.string)
            (decodePage 0)
            (Decode.field "number_of_pages" pageDecoder)
            (decodeISBN isbnString)
        )
    )


bookDecoder : Decoder Book
bookDecoder =
    (Decode.map5 Book
        (Decode.field "name" Decode.string)
        (Decode.field "by" Decode.string)
        (Decode.field "progress" pageDecoder)
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


debugAndReturn : a -> String -> b -> a
debugAndReturn a errString err =
    let
        _ =
            Debug.log errString err
    in
        a



-- Json encoders


encodeBook : Book -> Encode.Value
encodeBook book =
    Encode.object
        [ ( "name", Encode.string book.name )
        , ( "by", Encode.string book.by )
        , ( "progress", encodePage book.progress )
        , ( "pageCount", encodePage book.pageCount )
        , ( "isbn", encodeISBN book.isbn )
        ]


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
