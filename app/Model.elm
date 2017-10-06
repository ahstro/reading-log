module Model
    exposing
        ( Model
        , Book
        , ISBN(..)
        , Page(..)
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
