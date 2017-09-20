module Helpers.Page exposing (get, add, sub)

import Model exposing (Page(..))


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
