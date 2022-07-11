module Main exposing (LetterState(..), main, validateAttempt)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import FiveLetterWords exposing (getRandomWord, isValidWord, wordsLength)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, disabled, id, maxlength, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Random
import Task



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
    List Letter


type GameState
    = Loading
    | Playing String
    | Won String
    | Lost String
    | Error String


type LetterState
    = CorrectPlace
    | IncorrectPlace
    | NotIncluded
    | NotTried


type alias Letter =
    ( Char, LetterState )


type Msg
    = SubmitAttempt String (List (Maybe Char))
    | CharEntered Int (Maybe Char)
    | ClearAttempt
    | GenerateRandomIndex Int
    | NoOp


type alias Model =
    { history : List Attempt
    , currentAttempt : Array (Maybe Char)
    , state : GameState
    }


initalModel : Model
initalModel =
    { history = []
    , currentAttempt = emptyRow
    , state = Loading
    }


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



-- CONSTANTS


defaultRowLength : Int
defaultRowLength =
    5


maxiumAttempts : Int
maxiumAttempts =
    6


emptyRow : Array (Maybe Char)
emptyRow =
    Array.initialize defaultRowLength (\_ -> Nothing)



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "padding" "10px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "10px"
        ]
        (List.map
            historicRow
            model.history
            ++ [ case model.state of
                    Won _ ->
                        div [ style "font-size" "32px" ] [ text ("You won! You guessed the correct word in " ++ (model.history |> List.length |> String.fromInt) ++ " attempts") ]

                    Lost word ->
                        div [ style "font-size" "32px" ] [ text ("You lost! The word was " ++ String.toUpper word) ]

                    Playing word ->
                        activeRow word model.currentAttempt

                    Loading ->
                        div [] [ text "Loading!" ]

                    Error message ->
                        div [] [ text message ]
               , keyboardView model.history
               ]
        )


letterState : Char -> List Letter -> LetterState
letterState char letters =
    let
        states =
            letters
                |> List.filterMap
                    (\( c, s ) ->
                        if c == char then
                            Just s

                        else
                            Nothing
                    )
    in
    if List.member CorrectPlace states then
        CorrectPlace

    else if List.member IncorrectPlace states then
        IncorrectPlace

    else if List.member NotIncluded states then
        NotIncluded

    else
        NotTried


keyboardView : List Attempt -> Html Msg
keyboardView attempts =
    let
        letterList : List Letter
        letterList =
            List.concat attempts

        updatedAlphabet : List ( Char, LetterState )
        updatedAlphabet =
            List.map (\k -> ( k, letterState k letterList )) alphabet
    in
    div
        [ style "display" "flex"
        , style "flex-wrap" "wrap"
        , style "gap" "10px"
        ]
        (updatedAlphabet
            |> List.map
                (\( char, state ) ->
                    button
                        [ style "background-color" (backgroundColor state)
                        , style "font-size" "32px"
                        , style "width" "2em"
                        ]
                        [ text (char |> Char.toUpper |> String.fromChar) ]
                )
        )


historicRow : List Letter -> Html msg
historicRow attempt =
    div
        [ style "display" "flex"
        , style "gap" "10px"
        ]
        (attempt
            |> List.map
                (\letter ->
                    let
                        ( char, state ) =
                            letter
                    in
                    div
                        [ style "padding" "10px"
                        , style "border" "1px solid black"
                        , style "border-radius" "10px"
                        , style "text-transform" "uppercase"
                        , style "font-size" "32px"
                        , style "font-weight" "bold"
                        , style "text-align" "center"
                        , style "width" "1em"
                        , style "background-color" (backgroundColor state)
                        ]
                        [ text (String.fromChar char) ]
                )
        )


activeRow : String -> Array (Maybe Char) -> Html Msg
activeRow word attempt =
    let
        cells : List (Html Msg)
        cells =
            attempt
                |> Array.indexedMap
                    (\index ->
                        \char ->
                            div
                                [ style "padding" "10px"
                                , style "border" "1px solid black"
                                , style "border-radius" "10px"
                                , style "font-weight" "bold"
                                , style "width" "1em"
                                , style "font-size" "32px"
                                ]
                                [ input
                                    [ maxlength 1
                                    , style "width" "1em"
                                    , style "border" "0px"
                                    , style "font-size" "32px"
                                    , style "text-align" "center"
                                    , style "text-transform" "uppercase"
                                    , type_ "text"
                                    , id ("cell" ++ String.fromInt index)
                                    , autocomplete False
                                    , onInput
                                        (\str ->
                                            str
                                                |> String.toLower
                                                |> String.toList
                                                |> List.head
                                                |> CharEntered index
                                        )
                                    , value
                                        (case char of
                                            Just c ->
                                                String.fromChar c

                                            Nothing ->
                                                ""
                                        )
                                    ]
                                    []
                                ]
                    )
                |> Array.toList

        attemptList : List (Maybe Char)
        attemptList =
            Array.toList attempt

        canClearAttempt : Bool
        canClearAttempt =
            List.any
                (\cell ->
                    case cell of
                        Just _ ->
                            True

                        Nothing ->
                            False
                )
                attemptList

        canSubmitAttempt : Bool
        canSubmitAttempt =
            List.all
                (\cell ->
                    case cell of
                        Just _ ->
                            True

                        Nothing ->
                            False
                )
                attemptList
    in
    form
        [ style "display" "flex"
        , style "gap" "10px"
        , onSubmit (SubmitAttempt word (attempt |> Array.toList))
        ]
        (cells
            ++ [ button
                    [ disabled (not canSubmitAttempt)
                    , type_ "submit"
                    ]
                    [ text "Submit" ]
               , button
                    [ disabled (not canClearAttempt)
                    , onClick ClearAttempt
                    ]
                    [ text "Clear"
                    ]
               ]
        )


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


focusInput : String -> Cmd Msg
focusInput id =
    Task.attempt (\_ -> NoOp) (Dom.focus id)


focusFirstCell : Cmd Msg
focusFirstCell =
    focusInput "cell0"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitAttempt word [ Just a, Just b, Just c, Just d, Just e ] ->
            let
                attempt =
                    [ a, b, c, d, e ]

                validatedAttempt =
                    validateAttempt word attempt
            in
            case validatedAttempt of
                Nothing ->
                    -- TODO: Show alert that word was invalid
                    ( { model
                        | currentAttempt = emptyRow
                      }
                    , focusFirstCell
                    )

                Just validated ->
                    ( { model
                        | history = model.history ++ [ validated ]
                        , currentAttempt = emptyRow
                        , state =
                            if List.all (\( _, lS ) -> lS == CorrectPlace) validated then
                                Won word

                            else if List.length model.history + 1 == maxiumAttempts then
                                Lost word

                            else
                                Playing word
                      }
                    , focusFirstCell
                    )

        SubmitAttempt _ _ ->
            ( model, Cmd.none )

        CharEntered index (Just char) ->
            let
                charCode =
                    Char.toCode char

                focusNextCell =
                    focusInput ("cell" ++ String.fromInt (index + 1))
            in
            -- TODO: Allow backspace to remove current char
            if char == ' ' then
                ( model, focusNextCell )

            else if charCode < aCharCode || charCode > zCharCode then
                ( model, Cmd.none )

            else
                ( { model | currentAttempt = Array.set index (Just (Char.toLower char)) model.currentAttempt }
                , focusNextCell
                )

        CharEntered _ Nothing ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        GenerateRandomIndex index ->
            let
                word =
                    getRandomWord index
            in
            case word of
                Just w ->
                    ( { model | state = Playing w }, focusFirstCell )

                Nothing ->
                    ( { model | state = Error "Failed to get random word" }, Cmd.none )

        ClearAttempt ->
            ( { model | currentAttempt = emptyRow }, focusFirstCell )


validateAttempt : String -> List Char -> Maybe (List Letter)
validateAttempt correct attempt =
    -- Check first for correct place and remove these
    -- then check for incorrect place
    -- the rest are not included
    if isValidWord (String.fromList attempt) then
        Just
            (List.map2
                (\attemptChar ->
                    \correctChar ->
                        ( attemptChar
                        , validateChar
                            attemptChar
                            correctChar
                            correct
                        )
                )
                attempt
                (String.toList correct)
            )

    else
        Nothing


validateChar : Char -> Char -> String -> LetterState
validateChar attemptChar correctChar correct =
    if attemptChar == correctChar then
        CorrectPlace

    else if String.contains (String.fromChar attemptChar) correct then
        IncorrectPlace

    else
        NotIncluded



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initalModel, Random.generate GenerateRandomIndex (Random.int 0 wordsLength) )
        , view =
            \model ->
                { title = "Wordle"
                , body = [ view model ]
                }
        , update = \msg -> \model -> update msg model
        , subscriptions = \_ -> Sub.none
        }
