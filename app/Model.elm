module Model
    exposing
        ( Model
        , init
        , Book
        , ISBN(..)
        , Page(..)
        , addBook
        )

import EveryDict exposing (EveryDict)
import RemoteData exposing (WebData)


type ISBN
    = ISBN Int


type Page
    = Page Int


type alias Book =
    { name : String
    , by : String
    , pageCount : Page
    , isbn : ISBN
    }


type alias Model =
    { books : EveryDict ISBN Book
    , progress : EveryDict ISBN Page
    , daysToRead : EveryDict ISBN Int
    , bookToAdd : WebData Book
    , isbnToAdd : String
    }


init : ( Model, Cmd msg )
init =
    ( (addBook
        (Book
            "Regnet luktar inte här"
            "Duraid Al-Khamisi"
            (Page 205)
            (ISBN 9789173895606)
        )
        26
        { books = EveryDict.empty
        , progress = EveryDict.empty
        , daysToRead = EveryDict.empty
        , bookToAdd = RemoteData.NotAsked
        , isbnToAdd = ""
        }
      )
        |> (\model ->
                { model
                    | daysToRead =
                        EveryDict.insert
                            (ISBN 9789173895606)
                            30
                            model.daysToRead
                }
           )
    , Cmd.none
    )


addBook : Book -> Int -> Model -> Model
addBook book progress model =
    { model
        | books =
            EveryDict.insert
                book.isbn
                book
                model.books
        , progress =
            EveryDict.insert
                book.isbn
                (Page progress)
                model.progress
    }
