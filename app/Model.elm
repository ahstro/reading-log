module Model
    exposing
        ( Model
        , init
        , Book
        )


type Page
    = Page Int


type alias Book =
    { name : String
    , author : String
    , pageCount : ( Page, Page )
    }


type alias Model =
    { books : List Book
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
            ]
      }
    , Cmd.none
    )
