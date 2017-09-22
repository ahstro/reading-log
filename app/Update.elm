module Update
    exposing
        ( Msg(..)
        , update
        )

import Model
    exposing
        ( Model
        , Book
        , ISBN(..)
        , Page(..)
        , addBook
        )
import Http
import RemoteData exposing (WebData)
import Json.Decode as Decode


type Msg
    = NoOp
    | SetAddFormISBN String
    | FetchBook
    | HandleBookFetch (WebData Book)


updateModel : Model -> ( Model, Cmd Msg )
updateModel =
    (flip (!)) []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            updateModel model

        SetAddFormISBN isbn ->
            updateModel { model | isbnToAdd = isbn }

        FetchBook ->
            let
                -- isbnToAdd should always be a valid ISBN string here because
                -- it is enforced by the form input pattern attribute so we're
                -- just using it directly instead of en- and decoding first
                apiPath =
                    "https://openlibrary.org/api/books"
                        ++ "?bibkeys=ISBN:"
                        ++ model.isbnToAdd
                        ++ "&format=json"
                        ++ "&jscmd=data"
            in
                ( { model | bookToAdd = RemoteData.Loading }
                , Http.get apiPath (decodeBook model.isbnToAdd)
                    |> RemoteData.sendRequest
                    |> Cmd.map HandleBookFetch
                )

        HandleBookFetch res ->
            updateModel { model | bookToAdd = res }


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
