module View exposing (view)

import Html exposing (Html, div, text, img)
import Html.Attributes exposing (class, style, src)
import Model exposing (Model, ISBN, Book, Page(..))
import Update exposing (Msg(..))
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
        , div [] [ text book.author ]
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
