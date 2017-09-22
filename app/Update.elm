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


type Msg
    = NoOp
    | SetAddFormBy String
    | SetAddFormISBN String
    | SetAddFormName String
    | SetAddFormProgress String
    | SetAddFormPageCount String
    | AddBook


updateModel : Model -> ( Model, Cmd Msg )
updateModel model =
    -- (flip (!)) []
    let
        _ =
            Debug.log "FOO" model
    in
        model ! []


hideError : Model -> ( Model, Cmd Msg )
hideError model =
    updateModel { model | showError = False }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            updateModel model

        SetAddFormBy by ->
            hideError { model | addFormBy = by }

        SetAddFormISBN isbn ->
            hideError { model | addFormISBN = isbn }

        SetAddFormName name ->
            hideError { model | addFormName = name }

        SetAddFormProgress progress ->
            hideError { model | addFormProgress = progress }

        SetAddFormPageCount pageCount ->
            hideError { model | addFormPageCount = pageCount }

        AddBook ->
            let
                isbnRes =
                    model.addFormISBN
                        |> String.toInt

                progress =
                    model.addFormProgress
                        |> String.toInt
                        |> Result.withDefault 0

                pageCount =
                    model.addFormPageCount
                        |> String.toInt
                        |> Result.withDefault 0
            in
                case isbnRes of
                    Ok isbn ->
                        let
                            modelWithBook =
                                addBook
                                    (Book
                                        model.addFormName
                                        model.addFormBy
                                        (Page pageCount)
                                        (ISBN isbn)
                                    )
                                    progress
                                    model
                        in
                            updateModel
                                { modelWithBook
                                    | addFormBy = ""
                                    , addFormISBN = ""
                                    , addFormName = ""
                                    , addFormProgress = ""
                                    , addFormPageCount = ""
                                }

                    Err _ ->
                        updateModel { model | showError = True }
