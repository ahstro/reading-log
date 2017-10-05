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
    { books : List Book
    , progress : EveryDict ISBN Page
    }


init : ( Model, Cmd msg )
init =
    ( { books =
            [ Book
                "Regnet luktar inte här"
                "Duraid Al-Khamisi"
                ( Page 8
                , Page 205
                )
                (ISBN 9789173894944)
            ]
      , progress = EveryDict.singleton (ISBN 9789173894944) (Page 13)
      }
    , Cmd.none
    )
