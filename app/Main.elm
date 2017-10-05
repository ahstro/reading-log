module Main exposing (main)

import Html
import View exposing (view)
import Init exposing (init)
import Update exposing (update)


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
