module Init exposing (init, Flags)

import Model exposing (Model)
import Json exposing (booksDecoder, progressDecoder)
import Json.Decode
import RemoteData
import EveryDict


type alias Flags =
    Maybe
        { books : Json.Decode.Value
        , progress : Json.Decode.Value
        }


init : Flags -> ( Model, Cmd msg )
init maybeFlags =
    let
        books =
            maybeFlags
                |> Maybe.map .books
                |> Maybe.map (Json.Decode.decodeValue booksDecoder)
                |> Maybe.andThen Result.toMaybe
                |> Maybe.withDefault EveryDict.empty

        progress =
            maybeFlags
                |> Maybe.map .progress
                |> Maybe.map (Json.Decode.decodeValue progressDecoder)
                |> Maybe.andThen Result.toMaybe
                |> Maybe.withDefault EveryDict.empty
    in
        ( { books = books
          , progress = progress
          , daysToRead = EveryDict.empty
          , bookToAdd = RemoteData.NotAsked
          , isbnToAdd = ""
          }
        , Cmd.none
        )
