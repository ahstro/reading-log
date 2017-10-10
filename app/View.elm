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
import Model exposing (Model)
import Update exposing (Msg(..))
import RemoteData
import Book exposing (Book)
import Page exposing (Page)
import ISBN exposing (ISBN)
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
                |> List.map viewBook
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


viewBook : ( ISBN, Book ) -> Html Msg
viewBook ( isbn, book ) =
    div [ class "book" ]
        [ decorativeImg
            [ src <| getCover isbn
            , class "bookCover"
            ]
        , div [] [ text book.name ]
        , div [] [ text book.by ]
        , progressBar book.pageCount book.progress
        ]


progressBar : Page -> Page -> Html Msg
progressBar pageCount progress_ =
    let
        current =
            progress_
                |> Page.get
                |> toString

        pageCountString =
            pageCount
                |> Page.get
                |> toString
    in
        progress
            [ Html.Attributes.max pageCountString
            , value current
            ]
            [ text <| current ++ "/" ++ pageCountString ]


getCover : ISBN -> String
getCover isbn =
    let
        isbnString =
            isbn
                |> ISBN.get
                |> toString
    in
        "https://covers.openlibrary.org/b/isbn/" ++ isbnString ++ "-M.jpg"
