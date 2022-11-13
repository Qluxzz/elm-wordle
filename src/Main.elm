module Main exposing (LetterState(..), compareWords, main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import FiveLetterWords exposing (getRandomWord, isValidWord, wordsLength)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, class, disabled, id, maxlength, style, type_, value)
import Html.Events exposing (onClick, onFocus, onInput, onSubmit)
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
    | StartNewGame
    | GenerateRandomIndex Int
    | FocusedInput Int


type alias Model =
    { history : List Attempt
    , currentAttempt : Array (Maybe Char)
    , state : GameState
    , selectedCell : Int
    }


initalModel : Model
initalModel =
    { history = []
    , currentAttempt = emptyRow
    , state = Loading
    , selectedCell = 0
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


view : Model -> List (Html Msg)
view model =
    let
        playAgainButton : Html Msg
        playAgainButton =
            button [ onClick StartNewGame ] [ text "Play again!" ]
    in
    [ case model.state of
        Won _ ->
            let
                attempts : String
                attempts =
                    model.history
                        |> List.length
                        |> String.fromInt
            in
            div [ style "font-size" "32px" ]
                [ p [] [ text ("You won! You guessed the correct word in " ++ attempts ++ " attempts") ]
                , playAgainButton
                ]

        Lost word ->
            div [ style "font-size" "32px" ]
                [ p [] [ text ("You lost! The word was " ++ String.toUpper word) ]
                , playAgainButton
                ]

        Playing word ->
            div [ class "game" ]
                [ div
                    [ class "rows" ]
                    (List.map
                        historicRow
                        model.history
                        ++ [ activeRow word model.currentAttempt ]
                    )
                , keyboardView model.history model.currentAttempt word model.selectedCell
                ]

        Loading ->
            div [] [ text "Loading!" ]

        Error message ->
            div [] [ text message ]
    ]


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


qwerty : List (List Char)
qwerty =
    [ [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p' ]
    , [ 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l' ]
    , [ 'z', 'x', 'c', 'v', 'b', 'n', 'm' ]
    ]


keyboardView : List Attempt -> Array (Maybe Char) -> String -> Int -> Html Msg
keyboardView historicAttempts activeAttempt word focusedCellId =
    let
        letterList : List Letter
        letterList =
            List.concat historicAttempts

        updatedAlphabet : Dict Char LetterState
        updatedAlphabet =
            List.foldl (\k -> \acc -> Dict.insert k (letterState k letterList) acc) Dict.empty alphabet

        row : List Char -> List (Html Msg)
        row letters =
            letters
                |> List.map
                    (\char ->
                        button
                            [ style "background" (backgroundColor (Maybe.withDefault NotTried (Dict.get char updatedAlphabet)))
                            , onClick (CharEntered focusedCellId (Just char))
                            ]
                            [ text (char |> Char.toUpper |> String.fromChar) ]
                    )

        currentAttemptChars : List (Maybe Char)
        currentAttemptChars =
            Array.toList activeAttempt

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
                currentAttemptChars

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
                currentAttemptChars

        clearButton : Html Msg
        clearButton =
            button
                [ disabled (not canClearAttempt)
                , onClick ClearAttempt
                , type_ "button"
                ]
                [ text "Clear"
                ]

        submitButton : Html Msg
        submitButton =
            button
                [ disabled (not canSubmitAttempt)
                , onClick (SubmitAttempt word (activeAttempt |> Array.toList))
                , type_ "submit"
                ]
                [ text "Submit" ]
    in
    div [ class "keyboard" ]
        {- TODO: This is very ugly! -}
        [ div [ class "keyboard-row" ] (row (List.head qwerty |> Maybe.withDefault []))
        , div [ class "keyboard-row" ] (row (List.drop 1 qwerty |> List.head |> Maybe.withDefault []))
        , div [ class "keyboard-row" ]
            (clearButton
                :: row (List.drop 2 qwerty |> List.head |> Maybe.withDefault [])
                ++ [ submitButton ]
            )
        ]


historicRow : List Letter -> Html msg
historicRow attempt =
    div
        [ class "historic-row" ]
        (attempt
            |> List.map
                (\letter ->
                    let
                        ( char, state ) =
                            letter
                    in
                    div
                        [ style "background" (backgroundColor state) ]
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
                                    , onFocus (FocusedInput index)
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
    in
    form
        [ class "active-row"
        , onSubmit (SubmitAttempt word (attempt |> Array.toList))
        ]
        cells


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


focusCell : Int -> Cmd Msg
focusCell id =
    Task.attempt (\_ -> FocusedInput id) (Dom.focus ("cell" ++ String.fromInt id))


focusFirstCell : Cmd Msg
focusFirstCell =
    focusCell 0



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitAttempt word [ Just a, Just b, Just c, Just d, Just e ] ->
            let
                attempt =
                    [ a, b, c, d, e ]
            in
            case validateAttempt word attempt of
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
                    focusCell (index + 1)
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

        FocusedInput cellId ->
            ( { model | selectedCell = cellId }, Cmd.none )

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

        StartNewGame ->
            ( initalModel, Random.generate GenerateRandomIndex (Random.int 0 wordsLength) )


validateAttempt : String -> List Char -> Maybe (List Letter)
validateAttempt correct attempt =
    if not (isValidWord (String.fromList attempt)) then
        Nothing

    else
        Just (compareWords correct attempt)


compareWords : String -> List Char -> List Letter
compareWords correct attempt =
    let
        wordCharCount : Dict Char Int
        wordCharCount =
            List.foldl
                (\( correctChar, attemptChar ) ->
                    \c ->
                        if correctChar == attemptChar then
                            decreaseCount correctChar c

                        else
                            c
                )
                (String.foldl
                    (\char ->
                        \acc ->
                            Dict.update
                                char
                                (\mV ->
                                    Just
                                        (case mV of
                                            Just v ->
                                                v + 1

                                            Nothing ->
                                                1
                                        )
                                )
                                acc
                    )
                    Dict.empty
                    correct
                )
                (List.map2 Tuple.pair (String.toList correct) attempt)

        decreaseCount : Char -> Dict Char Int -> Dict Char Int
        decreaseCount char dict =
            Dict.update
                char
                (\mV ->
                    Just
                        (case mV of
                            Just v ->
                                Basics.max 0 (v - 1)

                            Nothing ->
                                0
                        )
                )
                dict

        charRemains : Char -> Dict Char Int -> Bool
        charRemains char dict =
            case Dict.get char dict of
                Just v ->
                    v > 0

                Nothing ->
                    False
    in
    Tuple.second
        (List.foldl
            (\( correctChar, attemptChar ) ->
                \( count, result ) ->
                    if correctChar == attemptChar then
                        ( count, result ++ [ ( attemptChar, CorrectPlace ) ] )

                    else if charRemains attemptChar count then
                        ( decreaseCount attemptChar count, result ++ [ ( attemptChar, IncorrectPlace ) ] )

                    else
                        ( count, result ++ [ ( attemptChar, NotIncluded ) ] )
            )
            ( wordCharCount, [] )
            (List.map2 Tuple.pair (String.toList correct) attempt)
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initalModel, Random.generate GenerateRandomIndex (Random.int 0 wordsLength) )
        , view =
            \model ->
                { title = "ELM Wordle"
                , body = view model
                }
        , update = \msg -> \model -> update msg model
        , subscriptions = \_ -> Sub.none
        }
