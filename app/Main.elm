module Main exposing (main)

import Html
import View exposing (view)
import Init exposing (init, Flags)
import Model exposing (Model)
import Update exposing (update, Msg)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
