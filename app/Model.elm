module Model
    exposing
        ( Model
        , Progress
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


type alias Books =
    EveryDict ISBN Book


type alias Progress =
    EveryDict ISBN Page


type alias Model =
    { books : Books
    , progress : Progress
    , bookToAdd : WebData Book
    , isbnToAdd : String
    }
