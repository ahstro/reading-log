module View exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Model exposing (Model)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ div [ class "books" ]
            (model.books
                |> EveryDict.toList
                |> List.map (viewBook model)
            )
        ]


viewBook : Model -> Book -> Html Msg
viewBook model book =
    div [ class "book" ]
        [ div [] [ text book.name ]
        , div [] [ text book.author ]
        ]
