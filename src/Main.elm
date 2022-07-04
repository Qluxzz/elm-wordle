module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (contenteditable, maxlength, style, value)
import Html.Events exposing (onInput)



{-
   Elm Wordle

   Wordle is a word game where you have six attempts
   to guess the correct five letter word

   If the character is in the correct spot of the correct word
   it is marked with a green background

   If the character appears in the word but is in the wrong
   place, it is marked with an orange background

   If the character does not appear in the word,
   it is marked with a gray background
-}


type alias Model =
    { tries : Int
    , history : List String
    , currentAttempt : String
    }


type Msg
    = Increment
    | Decrement
    | CharEntered String


initalModel : Model
initalModel =
    { tries = 0
    , history =
        [ "testa"
        ]
    , currentAttempt = "andel"
    }


word : String
word =
    "spela"


view : Model -> Html Msg
view model =
    div
        [ style "padding" "10px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "10px"
        ]
        (List.append
            (List.map
                row
                model.history
            )
            [ activeRow model.currentAttempt ]
        )


rowBase : Int -> String -> (Char -> Html Msg) -> Html Msg
rowBase rowLength attempt elem =
    let
        mapped : List ( Int, String )
        mapped =
            [ ( 1, "" ) ]
    in
    -- for (let i = 0; i < rowLength; ++i)
    --   attempt[i]
    div
        [ style "display" "flex"
        , style "gap" "10px"
        ]
        (List.range 1 rowLength
            |> List.map
                (\char ->
                    div
                        [ style "padding" "10px"
                        , style "border" "1px solid black"
                        , style "border-radius" "10px"
                        , style "text-transform" "uppercase"
                        , style "font-size" "32px"
                        , style "font-weight" "bold"
                        , style "text-align" "center"
                        , style "width" "1em"
                        ]
                        [ elem (Char.fromCode char) ]
                )
        )


row : String -> Html Msg
row attempt =
    rowBase 5 attempt (\char -> text (String.fromChar char))


activeRow : String -> Html Msg
activeRow attempt =
    rowBase 5
        attempt
        (\char ->
            div
                [ maxlength 1
                , style "width" "1em"
                , style "border" "0px"
                , style "font-size" "32px"
                , style "text-align" "center"
                , style "text-transform" "uppercase"
                , contenteditable True
                , onInput (\str -> CharEntered str)
                , value (String.fromChar char)
                ]
                []
        )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | tries = model.tries + 1 }

        Decrement ->
            { model | tries = model.tries - 1 }

        CharEntered char ->
            { model | currentAttempt = char }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initalModel
        , view = view
        , update = update
        }
