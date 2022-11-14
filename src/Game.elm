module Game exposing (Model, Msg(..), init, update, view)

import Alphabet
import Array exposing (Array)
import Browser.Dom as Dom
import Dict exposing (Dict)
import FiveLetterWords as FLW
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Process
import Task



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


type LetterState
    = CorrectPlace
    | IncorrectPlace
    | NotIncluded
    | NotTried


type alias Letter =
    ( Char, LetterState )


type alias Attempt =
    List Letter


type State
    = Playing
    | Lost
    | Won


type alias Model =
    { history : List Attempt
    , currentAttempt : Array (Maybe Char)
    , selectedCell : Int
    , correctWord : String
    , state : State
    , alert : Maybe String
    }


init : String -> ( Model, Cmd Msg )
init word =
    ( { history = []
      , currentAttempt = emptyRow
      , selectedCell = 0
      , correctWord = word
      , state = Playing
      , alert = Nothing
      }
    , focusFirstCell
    )


type Msg
    = SubmitAttempt
    | CharEntered Char
    | RemoveChar
    | FocusedInput Int
    | PlayAgain
    | ClearAlert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitAttempt ->
            case Array.toList model.currentAttempt of
                [ Just a, Just b, Just c, Just d, Just e ] ->
                    let
                        attempt =
                            [ a, b, c, d, e ]
                    in
                    case validateAttempt model.correctWord attempt of
                        Nothing ->
                            -- TODO: Show alert that word was invalid
                            ( { model
                                | currentAttempt = emptyRow
                                , alert = Just (List.foldl (\x -> \acc -> acc ++ String.fromChar x) "" attempt ++ " is not a valid word!")
                              }
                            , Cmd.batch
                                [ focusFirstCell
                                , Process.sleep 1500
                                    |> Task.perform (\_ -> ClearAlert)
                                ]
                            )

                        Just validated ->
                            let
                                updatedHistory =
                                    model.history ++ [ validated ]
                            in
                            ( { model
                                | history = updatedHistory
                                , currentAttempt = emptyRow
                                , state =
                                    if List.all (\( _, lS ) -> lS == CorrectPlace) validated then
                                        Won

                                    else if List.length updatedHistory == maxiumAttempts then
                                        Lost

                                    else
                                        Playing
                              }
                            , focusFirstCell
                            )

                _ ->
                    ( model, Cmd.none )

        CharEntered char ->
            let
                charCode =
                    Char.toCode char

                focusNextCell =
                    focusCell (model.selectedCell + 1)
            in
            -- TODO: Allow backspace to remove current char
            if char == ' ' then
                ( model, focusNextCell )

            else if charCode < Alphabet.aCharCode || charCode > Alphabet.zCharCode then
                ( model, Cmd.none )

            else
                ( { model | currentAttempt = Array.set model.selectedCell (Just (Char.toLower char)) model.currentAttempt }
                , focusNextCell
                )

        RemoveChar ->
            ( { model | currentAttempt = Array.set model.selectedCell Nothing model.currentAttempt }, focusCell (model.selectedCell - 1) )

        FocusedInput cellId ->
            ( { model | selectedCell = cellId }, Cmd.none )

        {- Handled by Main update method -}
        PlayAgain ->
            ( model, Cmd.none )

        ClearAlert ->
            ( { model | alert = Nothing }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        playAgainButton =
            button
                [ HE.onClick PlayAgain
                ]
                [ text "Play another round!" ]

        alertDialog =
            case model.alert of
                Just alert ->
                    div [ HA.class "alert" ] [ text alert ]

                Nothing ->
                    text ""
    in
    case model.state of
        Won ->
            div []
                [ h1 [] [ text "You won!" ]
                , playAgainButton
                ]

        Lost ->
            div [] [ h1 [] [ text ("You lost! The correct word was " ++ model.correctWord) ], playAgainButton ]

        Playing ->
            div [ HA.class "game" ]
                [ div
                    [ HA.class "rows" ]
                    (List.map
                        historicRow
                        model.history
                        ++ [ activeRow model.currentAttempt model.selectedCell ]
                    )
                , keyboardView model.history model.currentAttempt
                , alertDialog
                ]


keyboardView : List Attempt -> Array (Maybe Char) -> Html Msg
keyboardView historicAttempts activeAttempt =
    let
        letterList : List Letter
        letterList =
            List.concat historicAttempts

        updatedAlphabet : Dict Char LetterState
        updatedAlphabet =
            List.foldl (\k -> \acc -> Dict.insert k (letterState k letterList) acc) Dict.empty Alphabet.alphabet

        row : List Char -> List (Html Msg)
        row letters =
            letters
                |> List.map
                    (\char ->
                        button
                            [ HA.style "background" (backgroundColor (Maybe.withDefault NotTried (Dict.get char updatedAlphabet)))
                            , HE.onClick (CharEntered char)
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
                [ HA.disabled (not canClearAttempt)
                , HE.onClick RemoveChar
                , HA.type_ "button"
                ]
                [ text "Clear"
                ]

        submitButton : Html Msg
        submitButton =
            button
                [ HA.disabled (not canSubmitAttempt)
                , HE.onClick SubmitAttempt
                , HA.type_ "submit"
                ]
                [ text "Submit" ]
    in
    div [ HA.class "keyboard" ]
        [ div [ HA.class "keyboard-row" ] (row [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p' ])
        , div [ HA.class "keyboard-row" ] (row [ 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l' ])
        , div [ HA.class "keyboard-row" ]
            (clearButton
                :: row [ 'z', 'x', 'c', 'v', 'b', 'n', 'm' ]
                ++ [ submitButton ]
            )
        ]


historicRow : List Letter -> Html msg
historicRow attempt =
    div
        [ HA.class "historic-row" ]
        (attempt
            |> List.map
                (\letter ->
                    let
                        ( char, state ) =
                            letter
                    in
                    div
                        [ HA.style "background" (backgroundColor state) ]
                        [ text (String.fromChar char) ]
                )
        )


activeRow : Array (Maybe Char) -> Int -> Html Msg
activeRow attempt focusedIndex =
    let
        cells : List (Html Msg)
        cells =
            attempt
                |> Array.indexedMap
                    (\index ->
                        \char ->
                            div
                                [ HA.style "id" ("cell" ++ String.fromInt index)
                                , HA.classList [ ( "selected", focusedIndex == index ) ]
                                , HE.onFocus (FocusedInput index)
                                , HE.onClick (FocusedInput index)
                                , HA.tabindex 0
                                ]
                                [ text
                                    (char
                                        |> Maybe.map String.fromChar
                                        |> Maybe.withDefault ""
                                    )
                                ]
                    )
                |> Array.toList
    in
    form
        [ HA.class "active-row"
        , HE.onSubmit SubmitAttempt
        ]
        cells



-- HELPERS


focusCell : Int -> Cmd Msg
focusCell id =
    Task.attempt (\_ -> FocusedInput id) (Dom.focus ("cell" ++ String.fromInt id))


focusFirstCell : Cmd Msg
focusFirstCell =
    focusCell 0


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


validateAttempt : String -> List Char -> Maybe (List Letter)
validateAttempt correct attempt =
    if not (FLW.isValidWord (String.fromList attempt)) then
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
