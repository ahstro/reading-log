module Model exposing (Model, encodeModel)

import Book exposing (Book, Books, encodeBooks)
import RemoteData exposing (WebData)
import Json.Encode as Encode


-- Types


type alias Model =
    { books : Books
    , bookToAdd : WebData Book
    , isbnToAdd : String
    }



-- Json encoders


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "books", encodeBooks model.books )
        ]
