module Model
    exposing
        ( Model
        , init
        , Book
        )


type alias Book =
    { name : String
    , author : String
    , pageCount : ( Int, Int )
    }


type alias Model =
    { books : List Book
    }


init : ( Model, Cmd msg )
init =
    ( { books =
            [ Book "Regnet Luktar Inte Här" "Duraid Al-Khamisi" ( 8, 205 )
            ]
      }
    , Cmd.none
    )
