module Init exposing (init)

import Model exposing (Model)
import Json exposing (booksDecoder)
import Json.Decode
import RemoteData
import EveryDict


type alias Flags =
    Maybe
        { books : Json.Decode.Value
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
    in
        ( { books = books
          , progress = EveryDict.empty
          , daysToRead = EveryDict.empty
          , bookToAdd = RemoteData.NotAsked
          , isbnToAdd = ""
          }
        , Cmd.none
        )
