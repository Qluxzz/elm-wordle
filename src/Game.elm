module Game exposing
    ( LetterState(..)
    , Model
    , Msg(..)
    , compareWords
    , init
    , update
    , view
    )

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
    , triedLetterStates : Dict Char LetterState
    }


init : String -> ( Model, Cmd Msg )
init word =
    ( { history = []
      , currentAttempt = emptyRow
      , selectedCell = 0
      , correctWord = word
      , state = Playing
      , alert = Nothing
      , triedLetterStates = Dict.empty
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
                            ( { model
                                | currentAttempt = emptyRow
                                , alert =
                                    let
                                        word =
                                            List.foldr
                                                (String.fromChar >> (++))
                                                ""
                                                attempt
                                    in
                                    Just (word ++ " is not a valid word!")
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
                                , triedLetterStates =
                                    combineLetterStates
                                        model.triedLetterStates
                                        validated
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
                focusNextCell =
                    focusCell (model.selectedCell + 1)
            in
            ( { model
                | currentAttempt =
                    Array.set
                        model.selectedCell
                        (Just char)
                        model.currentAttempt
              }
            , focusNextCell
            )

        {-
           If the current cell is empty, clear previous cell and focus that cell
           Otherwise clear char in current cell
        -}
        RemoveChar ->
            let
                unwrappedValue : Maybe Char
                unwrappedValue =
                    Array.get
                        model.selectedCell
                        model.currentAttempt
                        |> Maybe.andThen (\v -> v)

                clearIndex : Int
                clearIndex =
                    case unwrappedValue of
                        Nothing ->
                            if model.selectedCell > 0 then
                                model.selectedCell - 1

                            else
                                model.selectedCell

                        Just _ ->
                            model.selectedCell
            in
            ( { model | currentAttempt = Array.set clearIndex Nothing model.currentAttempt }
            , focusCell clearIndex
            )

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
        alertDialog =
            case model.alert of
                Just alert ->
                    div [ HA.class "alert" ] [ text alert ]

                Nothing ->
                    text ""

        currentAttemptChars : List (Maybe Char)
        currentAttemptChars =
            Array.toList model.currentAttempt

        cellHasChar : Maybe Char -> Bool
        cellHasChar c =
            case c of
                Just _ ->
                    True

                Nothing ->
                    False

        canClearAttempt : Bool
        canClearAttempt =
            List.any
                cellHasChar
                currentAttemptChars

        canSubmitAttempt : Bool
        canSubmitAttempt =
            List.all
                cellHasChar
                currentAttemptChars
    in
    div [ HA.class "game" ]
        [ div
            [ HA.class "rows" ]
            (List.map
                historicRow
                model.history
                ++ [ activeRow model.currentAttempt model.selectedCell ]
            )
        , keyboardView model.triedLetterStates canSubmitAttempt canClearAttempt
        , alertDialog
        , case model.state of
            Won ->
                gameResultsView model.correctWord "You won!"

            Lost ->
                gameResultsView model.correctWord ("You lost! The correct word was " ++ model.correctWord)

            Playing ->
                text ""
        ]


playAgainButton : Html Msg
playAgainButton =
    button
        [ HE.onClick PlayAgain
        , HA.style "font-size" "20px"
        ]
        [ text "PLAY AGAIN" ]


gameResultsView : String -> String -> Html Msg
gameResultsView correctWord resultText =
    div [ HA.class "overlay" ]
        [ h1 [ HA.style "text-align" "center" ]
            [ text resultText ]
        , playAgainButton
        , a
            [ HA.href ("https://www.merriam-webster.com/dictionary/" ++ String.toLower correctWord)
            , HA.target "_blank"
            ]
            [ text ("Dictionary entry for '" ++ String.toLower correctWord ++ "'") ]
        ]


keyboardView : Dict Char LetterState -> Bool -> Bool -> Html Msg
keyboardView triedLetters canSubmitAttempt canClearAttempt =
    let
        row : List Char -> List (Html Msg)
        row letters =
            letters
                |> List.map
                    (\char ->
                        div
                            [ HA.style "background" (backgroundColor (Maybe.withDefault NotTried (Dict.get char triedLetters)))
                            , HE.onClick (CharEntered char)
                            ]
                            [ text (char |> Char.toUpper |> String.fromChar) ]
                    )

        clearButton : Html Msg
        clearButton =
            div
                [ HA.classList [ ( "disabled", not canClearAttempt ) ]
                , HE.onClick RemoveChar
                ]
                [ text "⬅️"
                ]

        submitButton : Html Msg
        submitButton =
            div
                [ HA.classList [ ( "disabled", not canSubmitAttempt ) ]
                , HE.onClick SubmitAttempt
                , HA.style "flex-shrink" "0"
                , HA.style "flex-basis" "34px"
                ]
                [ text "Submit" ]
    in
    div [ HA.class "keyboard" ]
        (List.map
            (div [ HA.class "keyboard-row" ])
            [ row [ 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P' ]
            , row [ 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L' ]
            , clearButton
                :: row [ 'Z', 'X', 'C', 'V', 'B', 'N', 'M' ]
                ++ [ submitButton ]
            ]
        )


historicRow : List Letter -> Html msg
historicRow attempt =
    div
        [ HA.class "historic-row" ]
        (attempt
            |> List.map
                (\( char, state ) ->
                    div
                        [ HA.style "background" (backgroundColor state) ]
                        [ text (String.fromChar char) ]
                )
        )


activeRow : Array (Maybe Char) -> Int -> Html Msg
activeRow attempt focusedIndex =
    div
        [ HA.class "active-row" ]
        (attempt
            |> Array.indexedMap
                (\index ->
                    \char ->
                        div
                            [ HA.style "id" ("cell" ++ String.fromInt index)
                            , HA.classList [ ( "selected", focusedIndex == index ) ]
                            , HE.onFocus (FocusedInput index)
                            , HE.onClick (FocusedInput index)
                            ]
                            [ text
                                (char
                                    |> Maybe.map String.fromChar
                                    |> Maybe.withDefault ""
                                )
                            ]
                )
            |> Array.toList
        )



-- HELPERS


focusCell : Int -> Cmd Msg
focusCell id =
    Task.attempt (\_ -> FocusedInput id) (Dom.focus ("cell" ++ String.fromInt id))


focusFirstCell : Cmd Msg
focusFirstCell =
    focusCell 0


backgroundColor : LetterState -> String
backgroundColor state =
    case state of
        NotIncluded ->
            "rgb(180, 180, 180)"

        CorrectPlace ->
            "rgb(0, 190, 0)"

        NotTried ->
            "rgb(225, 225, 225)"

        IncorrectPlace ->
            "#ffb01e"


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


letterStateOrder : LetterState -> Int
letterStateOrder x =
    case x of
        CorrectPlace ->
            0

        IncorrectPlace ->
            1

        NotIncluded ->
            2

        NotTried ->
            3


sortByLetterState : LetterState -> LetterState -> Order
sortByLetterState a b =
    compare (letterStateOrder a) (letterStateOrder b)


combineLetterStates : Dict Char LetterState -> Attempt -> Dict Char LetterState
combineLetterStates letterStates attemp =
    List.foldl
        (\( char, new ) ->
            \acc ->
                Dict.update char
                    (\maybe ->
                        Just
                            (case maybe of
                                Just old ->
                                    case sortByLetterState old new of
                                        LT ->
                                            old

                                        GT ->
                                            new

                                        EQ ->
                                            old

                                Nothing ->
                                    new
                            )
                    )
                    acc
        )
        letterStates
        attemp
