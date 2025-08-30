module List.Extra exposing (..)


all2 : (a -> b -> Bool) -> List a -> List b -> Bool
all2 pred xs ys =
    case ( xs, ys ) of
        ( [], [] ) ->
            True

        ( x :: xsTail, y :: ysTail ) ->
            pred x y && all2 pred xsTail ysTail

        _ ->
            False
