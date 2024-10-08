module Settings exposing (..)


type alias Settings =
    { clearInvalidAttempt : Bool -- If the user pressed submit and the attempt was not a word found in the dictionary, should the attempt be cleared or kept?
    }
