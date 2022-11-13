module Alphabet exposing (aCharCode, alphabet, zCharCode)


aCharCode : Int
aCharCode =
    97


zCharCode : Int
zCharCode =
    aCharCode + 25


alphabet : List Char
alphabet =
    List.range aCharCode zCharCode
        |> List.map (\i -> Char.fromCode i)
