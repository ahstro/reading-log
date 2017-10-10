port module Update
    exposing
        ( Msg(..)
        , update
        )

import Model exposing (Model, encodeModel)
import Book exposing (Book, decodeBookFromOpenLibrary, addBook)
import Page exposing (Page, pageDecoder)
import ISBN exposing (decodeISBN)
import Http
import RemoteData exposing (WebData)
import Json.Encode
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
                                |> addBook book

                        _ ->
                            model
            in
                updateModelAndSave { newModel | bookToAdd = res }


port updateLocalStorage : Json.Encode.Value -> Cmd msg
