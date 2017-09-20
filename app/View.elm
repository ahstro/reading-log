module View exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Model exposing (Model, ISBN, Book, Page(..))
import Update exposing (Msg(..))
import Helpers.Page as Page
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
viewBook model ( _, book ) =
    div [ class "book" ]
        [ div [] [ text book.name ]
        , div [] [ text book.author ]
        , progressBar book model
        ]


progressBar : Book -> Model -> Html Msg
progressBar book model =
    let
        ( start, end ) =
            book.pageCount

        current =
            model.progress
                |> EveryDict.get book.isbn
                |> Maybe.withDefault (Page 0)
                |> (flip Page.sub) start
                |> Page.get

        totalPages =
            Page.sub end start
                |> Page.get

        percent =
            totalPages
                |> toFloat
                |> (/) (toFloat current)
                |> (*) 100
                |> ceiling
                |> toString
                |> (flip (++)) "%"
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
            ]
