module Game exposing (Model, Msg, init, update, view)

import Alphabet
import Array exposing (Array)
import Browser.Dom as Dom
import Dict exposing (Dict)
import FiveLetterWords as FLW
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
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
    }


init : String -> ( Model, Cmd Msg )
init word =
    ( { history = []
      , currentAttempt = emptyRow
      , selectedCell = 0
      , correctWord = word
      , state = Playing
      }
    , focusFirstCell
    )


type Msg
    = SubmitAttempt (List (Maybe Char))
    | CharEntered Int (Maybe Char)
    | ClearAttempt
    | FocusedInput Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        word =
            model.correctWord
    in
    case msg of
        SubmitAttempt [ Just a, Just b, Just c, Just d, Just e ] ->
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
                                Won

                            else if List.length model.history + 1 == maxiumAttempts then
                                Lost

                            else
                                Playing
                      }
                    , focusFirstCell
                    )

        SubmitAttempt _ ->
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

            else if charCode < Alphabet.aCharCode || charCode > Alphabet.zCharCode then
                ( model, Cmd.none )

            else
                ( { model | currentAttempt = Array.set index (Just (Char.toLower char)) model.currentAttempt }
                , focusNextCell
                )

        CharEntered _ Nothing ->
            ( model, Cmd.none )

        FocusedInput cellId ->
            ( { model | selectedCell = cellId }, Cmd.none )

        ClearAttempt ->
            ( { model | currentAttempt = emptyRow }, focusFirstCell )



-- VIEW


view : Model -> Html Msg
view model =
    div [ HA.class "game" ]
        [ div
            [ HA.class "rows" ]
            (List.map
                historicRow
                model.history
                ++ [ activeRow model.currentAttempt ]
            )
        , keyboardView model.history model.currentAttempt model.selectedCell
        ]


qwerty : List (List Char)
qwerty =
    [ [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p' ]
    , [ 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l' ]
    , [ 'z', 'x', 'c', 'v', 'b', 'n', 'm' ]
    ]


keyboardView : List Attempt -> Array (Maybe Char) -> Int -> Html Msg
keyboardView historicAttempts activeAttempt focusedCellId =
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
                            , HE.onClick (CharEntered focusedCellId (Just char))
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
                , HE.onClick ClearAttempt
                , HA.type_ "button"
                ]
                [ text "Clear"
                ]

        submitButton : Html Msg
        submitButton =
            button
                [ HA.disabled (not canSubmitAttempt)
                , HE.onClick (SubmitAttempt (activeAttempt |> Array.toList))
                , HA.type_ "submit"
                ]
                [ text "Submit" ]
    in
    div [ HA.class "keyboard" ]
        {- TODO: This is very ugly! -}
        [ div [ HA.class "keyboard-row" ] (row (List.head qwerty |> Maybe.withDefault []))
        , div [ HA.class "keyboard-row" ] (row (List.drop 1 qwerty |> List.head |> Maybe.withDefault []))
        , div [ HA.class "keyboard-row" ]
            (clearButton
                :: row (List.drop 2 qwerty |> List.head |> Maybe.withDefault [])
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


activeRow : Array (Maybe Char) -> Html Msg
activeRow attempt =
    let
        cells : List (Html Msg)
        cells =
            attempt
                |> Array.indexedMap
                    (\index ->
                        \char ->
                            div
                                [ HA.style "padding" "10px"
                                , HA.style "border" "1px solid black"
                                , HA.style "border-radius" "10px"
                                , HA.style "font-weight" "bold"
                                , HA.style "width" "1em"
                                , HA.style "font-size" "32px"
                                ]
                                [ input
                                    [ HA.maxlength 1
                                    , HA.style "width" "1em"
                                    , HA.style "border" "0px"
                                    , HA.style "font-size" "32px"
                                    , HA.style "text-align" "center"
                                    , HA.style "text-transform" "uppercase"
                                    , HA.type_ "text"
                                    , HA.id ("cell" ++ String.fromInt index)
                                    , HA.autocomplete False
                                    , HE.onFocus (FocusedInput index)
                                    , HE.onInput
                                        (\str ->
                                            str
                                                |> String.toLower
                                                |> String.toList
                                                |> List.head
                                                |> CharEntered index
                                        )
                                    , HA.value
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
        [ HA.class "active-row"
        , HE.onSubmit (SubmitAttempt (attempt |> Array.toList))
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
