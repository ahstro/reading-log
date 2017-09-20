module View exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Model exposing (Model)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ div [ class "books" ] <|
            List.map
                (\book ->
                    div [ class "book" ]
                        [ div [] [ text book.name ]
                        , div [] [ text book.author ]
                        ]
                )
                model.books
        ]
