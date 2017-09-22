module View exposing (view)

import Html exposing (Html, div, text, img, input, form, label, span, button)
import Html.Attributes
    exposing
        ( class
        , style
        , src
        , type_
        , name
        , pattern
        , disabled
        , required
        , placeholder
        , defaultValue
        )
import Html.Events exposing (onInput, onSubmit)
import Model exposing (Model, ISBN, Book, Page(..))
import Update exposing (Msg(..))
import RemoteData
import Helpers.Page as Page
import Helpers.ISBN as ISBN
import EveryDict


view : Model -> Html Msg
view model =
    div []
        [ div [ class "books" ]
            (model.books
                |> EveryDict.toList
                |> List.map (viewBook model)
            )
        , addBookForm model
        ]


addBookForm : Model -> Html Msg
addBookForm model =
    let
        ( buttonText, submitIsDisabled, onSubmit_ ) =
            case model.bookToAdd of
                RemoteData.NotAsked ->
                    ( "Fetch book", False, FetchBook )

                RemoteData.Loading ->
                    ( "Fetching book", True, NoOp )

                RemoteData.Success _ ->
                    ( "Add book", False, NoOp )

                RemoteData.Failure _ ->
                    ( "Try again", False, FetchBook )
    in
        form [ class "addBook", onSubmit onSubmit_ ]
            [ label [ class "addFormField" ]
                [ span [ class "addFormLabel addFormText" ] [ text "ISBN:" ]
                , input
                    [ type_ "text"
                    , placeholder "9780963009609"
                    , onInput SetAddFormISBN
                    , class "addFormInput"
                    , pattern "(\\d{10}|\\d{13})"
                    , required True
                    ]
                    []
                ]
            , button [ type_ "submit", disabled submitIsDisabled ] [ text buttonText ]
            ]


addBookField : String -> String -> String -> (String -> Msg) -> Html Msg
addBookField label_ type__ placeholder_ onInput_ =
    label [ class "addFormField" ]
        [ span [ class "addFormLabel addFormText" ] [ text label_ ]
        , input
            [ type_ type__
            , placeholder placeholder_
            , onInput onInput_
            , class "addFormInput"
            ]
            []
        ]


viewBook : Model -> ( ISBN, Book ) -> Html Msg
viewBook model ( isbn, book ) =
    div [ class "book" ]
        [ img
            [ src <| getCover isbn
            , class "bookCover"
            ]
            []
        , div [] [ text book.name ]
        , div [] [ text book.by ]
        , progressBar book model
        ]


progressBar : Book -> Model -> Html Msg
progressBar book model =
    let
        current =
            model.progress
                |> EveryDict.get book.isbn
                |> Maybe.withDefault (Page 0)
                |> Page.get

        percent =
            book.pageCount
                |> Page.get
                |> toFloat
                |> (/) (toFloat current)
                |> (*) 100
                |> ceiling
                |> toString
                |> (flip (++)) "%"

        daysToRead =
            model.daysToRead
                |> EveryDict.get book.isbn
    in
        div [ class "progressBar" ]
            [ div
                [ class "progress"
                , style
                    [ ( "width", percent )
                    ]
                ]
                []
            , div [ class "percent" ] [ text percent ]
            , milestones daysToRead
            ]


milestones : Maybe Int -> Html Msg
milestones daysToRead =
    case daysToRead of
        Just days ->
            div [ class "milestones" ]
                (List.repeat days <|
                    div [ class "milestone" ] []
                )

        Nothing ->
            text ""


getCover : ISBN -> String
getCover isbn =
    let
        isbnString =
            isbn
                |> ISBN.get
                |> toString
    in
        "https://covers.openlibrary.org/b/isbn/" ++ isbnString ++ "-M.jpg"
