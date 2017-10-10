module Page
    exposing
        ( Page(..)
        , get
        , add
        , sub
        , pageDecoder
        , decodePage
        , encodePage
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


-- Types


type Page
    = Page Int



-- Helpers


get : Page -> Int
get (Page p) =
    p


map2 : (Int -> Int -> Int) -> Page -> Page -> Page
map2 f (Page p1) (Page p2) =
    Page (f p1 p2)


add : Page -> Page -> Page
add =
    map2 (+)


sub : Page -> Page -> Page
sub =
    map2 (-)



-- JSON decoders


pageDecoder : Decoder Page
pageDecoder =
    Decode.int
        |> Decode.andThen (Decode.succeed << Page)


decodePage : Int -> Decoder Page
decodePage pageNo =
    Decode.succeed (Page pageNo)



-- JSON encoders


encodePage : Page -> Encode.Value
encodePage (Page page) =
    Encode.int page
