port module Update
    exposing
        ( Msg(..)
        , update
        )

import Model
    exposing
        ( Model
        , Book
        , Page(..)
        )
import Http
import RemoteData exposing (WebData)
import Json.Encode
import Json exposing (decodeISBN, decodeBookFromOpenLibrary, pageDecoder, encodeModel)
import EveryDict


type Msg
    = NoOp
    | SetAddFormISBN String
    | FetchBook
    | HandleBookFetch (WebData Book)


updateModel : Model -> ( Model, Cmd Msg )
updateModel =
    (flip (!)) []


updateModelAndSave : Model -> ( Model, Cmd Msg )
updateModelAndSave model =
    ( model, updateLocalStorage <| encodeModel model )


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
                , Http.get apiPath (decodeBookFromOpenLibrary model.isbnToAdd)
                    |> RemoteData.sendRequest
                    |> Cmd.map HandleBookFetch
                )

        HandleBookFetch res ->
            let
                newModel =
                    case res of
                        RemoteData.Success book ->
                            { model | isbnToAdd = "" }
                                |> addBook book 0

                        _ ->
                            model
            in
                updateModelAndSave { newModel | bookToAdd = res }


addBook : Book -> Int -> Model -> Model
addBook book progress model =
    { model
        | books =
            EveryDict.insert
                book.isbn
                book
                model.books
        , progress =
            EveryDict.insert
                book.isbn
                (Page progress)
                model.progress
    }


port updateLocalStorage : Json.Encode.Value -> Cmd msg
