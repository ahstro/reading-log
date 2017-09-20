module Model
    exposing
        ( Model
        , init
        , Book
        , ISBN(..)
        , Page(..)
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
    , daysToRead : EveryDict ISBN Int
    }


init : ( Model, Cmd msg )
init =
    ( (addBook
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
        , daysToRead = EveryDict.empty
        }
      )
        |> (\model ->
                { model
                    | progress =
                        EveryDict.insert
                            (ISBN 9789173894944)
                            (Page 21)
                            model.progress
                    , daysToRead =
                        EveryDict.insert
                            (ISBN 9789173894944)
                            30
                            model.daysToRead
                }
           )
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
