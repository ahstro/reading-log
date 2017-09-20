module Update
    exposing
        ( Msg(..)
        , update
        )

import Model exposing (Model, Book)


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
