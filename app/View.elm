module View exposing (view)

import Accessibility as Html
    exposing
        ( Html
        , main_
        , div
        , text
        , decorativeImg
        , inputText
        , form
        , labelBefore
        , span
        , button
        , aside
        , section
        , progress
        )
import Html.Attributes
    exposing
        ( class
        , style
        , src
        , type_
        , name
        , value
        , pattern
        , disabled
        , required
        , placeholder
        , defaultValue
        )
import Html.Events exposing (onInput, onWithOptions)
import Model
    exposing
        ( Model
        , Progress
        , ISBN
        , Book
        , Page(..)
        )
import Update exposing (Msg(..))
import RemoteData
import Helpers.Page as Page
import Helpers.ISBN as ISBN
import Json.Decode
import EveryDict


view : Model -> Html Msg
view model =
    main_ [ class "main" ]
        [ aside [ class "sidebar" ]
            [ addBookForm model
            ]
        , section [ class "books" ]
            (model.books
                |> EveryDict.toList
                |> List.map (viewBook model.progress)
            )
        ]


addBookForm : Model -> Html Msg
addBookForm model =
    let
        ( buttonText, submitIsDisabled, onClickMsg ) =
            case model.bookToAdd of
                RemoteData.NotAsked ->
                    ( "Add book", False, FetchBook )

                RemoteData.Loading ->
                    ( "Adding book", True, NoOp )

                RemoteData.Success _ ->
                    ( "Add another book", False, FetchBook )

                RemoteData.Failure _ ->
                    ( "Try again", False, FetchBook )
    in
        form [ class "addBook" ]
            [ labelBefore
                [ class "addFormField" ]
                (span [ class "addFormLabel addFormText" ] [ text "ISBN:" ])
                (inputText
                    model.isbnToAdd
                    [ placeholder "9780963009609"
                    , onInput SetAddFormISBN
                    , class "addFormInput"
                    , pattern "(\\d{10}|\\d{13})"
                    , required True
                    ]
                )
            , button
                [ class "addBookButton"
                , disabled submitIsDisabled
                , onWithOptions
                    "click"
                    { stopPropagation = True
                    , preventDefault = True
                    }
                    (Json.Decode.succeed onClickMsg)
                ]
                [ text buttonText ]
            ]


viewBook : Progress -> ( ISBN, Book ) -> Html Msg
viewBook progress ( isbn, book ) =
    div [ class "book" ]
        [ decorativeImg
            [ src <| getCover isbn
            , class "bookCover"
            ]
        , div [] [ text book.name ]
        , div [] [ text book.by ]
        , progressBar book progress
        ]


progressBar : Book -> Progress -> Html Msg
progressBar book progress_ =
    let
        current =
            progress_
                |> EveryDict.get book.isbn
                |> Maybe.withDefault (Page 0)
                |> Page.get
                |> toString

        pageCount =
            book.pageCount
                |> Page.get
                |> toString
    in
        progress
            [ Html.Attributes.max pageCount
            , value current
            ]
            [ text <| current ++ "/" ++ pageCount ]


getCover : ISBN -> String
getCover isbn =
    let
        isbnString =
            isbn
                |> ISBN.get
                |> toString
    in
        "https://covers.openlibrary.org/b/isbn/" ++ isbnString ++ "-M.jpg"
