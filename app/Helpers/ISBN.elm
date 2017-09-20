module Helpers.ISBN exposing (get)

import Model exposing (ISBN(..))


get : ISBN -> Int
get (ISBN isbn) =
    isbn
