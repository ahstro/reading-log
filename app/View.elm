module View exposing (view)

import Html exposing (Html, div, text)
import Model exposing (Model)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div [] <|
        List.map
            (\book ->
                div []
                    [ text book.name
                    , text book.author
                    ]
            )
            model.books
