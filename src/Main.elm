module Main exposing (..)

import Array
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (disabled, maxlength, style, type_, value)
import Html.Events exposing (onClick)



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


type alias Attempt =
    List Char


type alias Model =
    { history : List (List Letter)
    , currentAttempt : Attempt
    , letters : Dict Char LetterState
    }


type LetterState
    = CorrectPlace
    | IncorrectPlace
    | NotIncluded
    | NotTried


type alias Letter =
    ( Char, LetterState )


type Msg
    = SubmitAttempt
    | CharEntered Char



-- A list of all characters from A-Z


alphabet : List Char
alphabet =
    List.range 0 25 |> List.map (\i -> Char.fromCode (65 + i))


initalModel : Model
initalModel =
    { history =
        [ [ ( 'H', NotIncluded ), ( 'U', CorrectPlace ), ( 'M', NotIncluded ), ( 'A', NotIncluded ), ( 'N', NotIncluded ) ]
        ]
    , currentAttempt = []
    , letters = Dict.empty
    }


word : String
word =
    String.toUpper "GUESS"


view : Model -> Html Msg
view model =
    div
        [ style "padding" "10px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "10px"
        ]
        (List.map
            row
            model.history
            ++ [ activeRow model.currentAttempt
               , alphabetView (List.map (\letter -> ( letter, NotTried )) alphabet)
               ]
        )


rowBase : Int -> List Char -> (Char -> Html Msg) -> Html Msg
rowBase rowLength attempt elem =
    let
        arr =
            attempt |> Array.fromList
    in
    div
        [ style "display" "flex"
        , style "gap" "10px"
        ]
        (List.range 0 (rowLength - 1)
            |> List.map
                (\index ->
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
                        [ elem (arr |> Array.get index |> Maybe.withDefault ' ') ]
                )
        )


defaultRowLength : Int
defaultRowLength =
    5


row : List Letter -> Html msg
row attempt =
    div
        [ style "display" "flex"
        , style "gap" "10px"
        ]
        (attempt
            |> List.map
                (\letter ->
                    div
                        [ style "padding" "10px"
                        , style "border" "1px solid black"
                        , style "border-radius" "10px"
                        , style "text-transform" "uppercase"
                        , style "font-size" "32px"
                        , style "font-weight" "bold"
                        , style "text-align" "center"
                        , style "width" "1em"
                        , style "background-color" (backgroundColor (Tuple.second letter))
                        ]
                        [ text (String.fromChar (Tuple.first letter)) ]
                )
        )


activeRow : List Char -> Html Msg
activeRow attempt =
    div
        [ style "display" "flex"
        , style "gap" "10px"
        ]
        [ rowBase defaultRowLength
            attempt
            (\char ->
                input
                    [ maxlength 1
                    , style "width" "1em"
                    , style "border" "0px"
                    , style "font-size" "32px"
                    , style "text-align" "center"
                    , style "text-transform" "uppercase"
                    , type_ "text"
                    , value (String.fromChar char)
                    ]
                    []
            )
        , button [ onClick SubmitAttempt, disabled (List.length attempt < defaultRowLength) ] [ text "Submit" ]
        ]


alphabetView : List Letter -> Html Msg
alphabetView a =
    div
        [ style "display" "flex"
        , style "gap" "10px"
        , style "flex-wrap" "wrap"
        ]
        (List.map (\letter -> letterView letter) a)


backgroundColor : LetterState -> String
backgroundColor state =
    case state of
        NotIncluded ->
            "grey"

        CorrectPlace ->
            "green"

        NotTried ->
            "white"

        IncorrectPlace ->
            "orange"


letterView : Letter -> Html Msg
letterView letter =
    let
        char =
            Tuple.first letter
    in
    button
        [ style "background-color" (backgroundColor (Tuple.second letter))
        , style "font-size" "24px"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , onClick (CharEntered char)
        ]
        [ text (String.fromChar char) ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        SubmitAttempt ->
            { model
                | history = model.history ++ [ validateAttempt model.currentAttempt word ]
                , currentAttempt = []
            }

        CharEntered char ->
            { model | currentAttempt = model.currentAttempt ++ [ char ] }


validateAttempt : List Char -> String -> List Letter
validateAttempt attempt correct =
    List.map2 (\attemptChar -> \correctChar -> ( attemptChar, validateChar attemptChar correctChar correct )) attempt (String.toList correct)


validateChar : Char -> Char -> String -> LetterState
validateChar attemptChar correctChar correct =
    if attemptChar == correctChar then
        CorrectPlace

    else if String.contains (String.fromChar attemptChar) correct then
        IncorrectPlace

    else
        NotIncluded


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initalModel
        , view = view
        , update = update
        }
