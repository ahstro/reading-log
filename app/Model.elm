module Model
    exposing
        ( Model
        , init
        , Book
        )

import EveryDict exposing (EveryDict)


type ISBN
    = ISBN Int


type Page
    = Page Int


type alias Book =
    { name : String
    , author : String
    , pageCount : ( Page, Page )
    , isbn : ISBN
    }


type alias Model =
    { books : EveryDict ISBN Book
    , progress : EveryDict ISBN Page
    }


init : ( Model, Cmd msg )
init =
    ( addBook
        (Book
            "Regnet luktar inte här"
            "Duraid Al-Khamisi"
            ( Page 8
            , Page 205
            )
            (ISBN 9789173894944)
        )
        { books = EveryDict.empty
        , progress = EveryDict.empty
        }
    , Cmd.none
    )


addBook : Book -> Model -> Model
addBook book model =
    { model
        | books =
            EveryDict.insert
                book.isbn
                book
                model.books
        , progress =
            EveryDict.insert
                book.isbn
                (Tuple.first book.pageCount)
                model.progress
    }
