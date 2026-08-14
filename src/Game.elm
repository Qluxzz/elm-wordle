port module Game exposing
    ( Effect
    , LetterState(..)
    , Model
    , Msg(..)
    , SavedState
    , State(..)
    , compareWords
    , init
    , matchingWords
    , perform
    , update
    , view
    )

import Array exposing (Array)
import Browser.Dom
import Dict exposing (Dict)
import FiveLetterWords
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Process
import Set
import Settings exposing (Settings)
import Task



-- PORTS


port saveHistory : List String -> Cmd msg



-- CONSTANTS


defaultRowLength : Int
defaultRowLength =
    5


maximumAttempts : Int
maximumAttempts =
    6


emptyRow : Array (Maybe Char)
emptyRow =
    Array.initialize defaultRowLength (\_ -> Nothing)


type LetterState
    = CorrectPlace
    | IncorrectPlace
    | NotIncluded


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


type alias SavedState =
    List String


initFromSavedState : String -> List String -> { history : List (List Letter), state : State, triedLetterStates : Dict Char LetterState }
initFromSavedState word attempts =
    let
        history =
            attempts
                |> List.map String.toList
                |> List.map (compareWords word)

        triedLetterStates =
            combineLetterStates Dict.empty (List.concat history)
    in
    { history = history
    , state =
        if history |> List.any (List.all (\( _, lS ) -> lS == CorrectPlace)) then
            Won

        else if List.length history == maximumAttempts then
            Lost

        else
            Playing
    , triedLetterStates = triedLetterStates
    }


init : String -> Maybe SavedState -> ( Model, Effect )
init word savedState =
    let
        baseState =
            { history = []
            , currentAttempt = emptyRow
            , correctWord = word
            , selectedCell = 0
            , state = Playing
            , triedLetterStates = Dict.empty
            , alert = Nothing
            }
    in
    ( case savedState of
        Just saved ->
            let
                { history, state, triedLetterStates } =
                    initFromSavedState word saved
            in
            { baseState
                | history = history
                , state = state
                , triedLetterStates = triedLetterStates
            }

        Nothing ->
            baseState
    , FocusCell 0
    )


type Msg
    = SubmitAttempt
    | CharEntered Char
    | RemoveChar
    | FocusedInput Int
    | AlertCleared
    | FocusPreviousCell
    | FocusNextCell


type Effect
    = None
    | FocusCell Int
    | SaveHistory (List String)
    | ClearAlert
    | Batch (List Effect)


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        None ->
            Cmd.none

        FocusCell id ->
            focusCell id

        SaveHistory attempts ->
            saveHistory attempts

        ClearAlert ->
            clearAlert

        Batch effects ->
            effects |> List.map perform |> Cmd.batch


update : Msg -> Model -> Settings -> ( Model, Effect )
update msg model settings =
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
                            if settings.clearInvalidAttempt then
                                ( { model
                                    | currentAttempt = emptyRow
                                    , alert = createAlert attempt
                                  }
                                , Batch [ FocusCell 0, ClearAlert ]
                                )

                            else
                                ( { model | alert = createAlert attempt }, ClearAlert )

                        Just validated ->
                            let
                                updatedHistory =
                                    model.history ++ [ validated ]

                                newState =
                                    if List.all (\( _, lS ) -> lS == CorrectPlace) validated then
                                        Won

                                    else if List.length updatedHistory == maximumAttempts then
                                        Lost

                                    else
                                        Playing
                            in
                            ( { model
                                | history = updatedHistory
                                , currentAttempt = emptyRow
                                , triedLetterStates =
                                    combineLetterStates
                                        model.triedLetterStates
                                        validated
                                , state = newState
                                , alert =
                                    if newState /= Playing then
                                        Nothing

                                    else
                                        Just <| "Just " ++ String.fromInt (matchingWords updatedHistory |> Set.size) ++ "/" ++ String.fromInt FiveLetterWords.wordsLength ++ " matching words left!"
                              }
                            , Batch
                                [ FocusCell 0
                                , SaveHistory (List.map convertAttemptToString updatedHistory)
                                , ClearAlert
                                ]
                            )

                _ ->
                    ( model, None )

        CharEntered char ->
            ( { model
                | currentAttempt =
                    Array.set
                        model.selectedCell
                        (Just char)
                        model.currentAttempt
              }
            , FocusCell (nextCell model.selectedCell)
            )

        {-
           If the current cell is empty, clear previous cell and focus that cell
           Otherwise clear char in current cell
        -}
        RemoveChar ->
            let
                clearIndex =
                    case Array.get model.selectedCell model.currentAttempt of
                        Just (Just _) ->
                            model.selectedCell

                        _ ->
                            previousCell model.selectedCell
            in
            ( { model | currentAttempt = Array.set clearIndex Nothing model.currentAttempt }
            , FocusCell clearIndex
            )

        FocusedInput cellId ->
            ( { model | selectedCell = cellId }, None )

        FocusPreviousCell ->
            ( model
            , FocusCell (previousCell model.selectedCell)
            )

        FocusNextCell ->
            ( model
            , FocusCell (nextCell model.selectedCell)
            )

        AlertCleared ->
            ( { model | alert = Nothing }, None )



-- VIEW


view : Model -> Html Msg
view model =
    let
        currentAttemptChars : List (Maybe Char)
        currentAttemptChars =
            Array.toList model.currentAttempt

        rows =
            List.map
                historicRow
                model.history
                ++ List.filterMap identity
                    [ if model.state == Playing then
                        Just (activeRow model.currentAttempt model.selectedCell)

                      else
                        Nothing
                    ]
    in
    Html.div [ Html.Attributes.class "game" ]
        [ Html.div
            [ Html.Attributes.class "rows" ]
            (rows ++ List.repeat (maximumAttempts - List.length rows) emptyRowView)
        , keyboardView
            model.triedLetterStates
            (canSubmitAttempt currentAttemptChars)
            (canClearAttempt currentAttemptChars)
        , case model.alert of
            Just alert ->
                Html.div [ Html.Attributes.class "alert" ] [ Html.text alert ]

            Nothing ->
                Html.text ""
        ]


keyboardView : Dict Char LetterState -> Bool -> Bool -> Html Msg
keyboardView triedLetters canSubmit canClear =
    let
        row : List Char -> List (Html Msg)
        row letters =
            letters
                |> List.map
                    (\char ->
                        Html.div
                            (Html.Events.onClick (CharEntered char)
                                :: (case Dict.get char triedLetters of
                                        Just c ->
                                            [ Html.Attributes.style "background" (backgroundColor c) ]

                                        Nothing ->
                                            []
                                   )
                            )
                            [ Html.text (char |> Char.toUpper |> String.fromChar) ]
                    )

        clearButton : Html Msg
        clearButton =
            Html.div
                [ Html.Attributes.classList [ ( "disabled", not canClear ) ]
                , Html.Events.onClick RemoveChar
                ]
                [ Html.text "⬅️"
                ]

        submitButton : Html Msg
        submitButton =
            Html.div
                [ Html.Attributes.classList [ ( "disabled", not canSubmit ) ]
                , Html.Events.onClick SubmitAttempt
                , Html.Attributes.style "flex-shrink" "0"
                , Html.Attributes.style "flex-basis" "34px"
                ]
                [ Html.text "Submit" ]
    in
    Html.div [ Html.Attributes.class "keyboard" ]
        (List.map
            (Html.div [ Html.Attributes.class "keyboard-row" ])
            [ row [ 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P' ]
            , row [ 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L' ]
            , clearButton
                :: row [ 'Z', 'X', 'C', 'V', 'B', 'N', 'M' ]
                ++ [ submitButton ]
            ]
        )


historicRow : List Letter -> Html msg
historicRow attempt =
    Html.div
        [ Html.Attributes.class "historic-row" ]
        (attempt
            |> List.map
                (\( char, state ) ->
                    Html.div
                        [ Html.Attributes.style "background" (backgroundColor state) ]
                        [ Html.text (String.fromChar char) ]
                )
        )


activeRow : Array (Maybe Char) -> Int -> Html Msg
activeRow attempt focusedIndex =
    Html.div
        [ Html.Attributes.class "active-row" ]
        (attempt
            |> Array.indexedMap
                (\index ->
                    \char ->
                        Html.div
                            [ Html.Attributes.id <| "cell" ++ String.fromInt index
                            , Html.Attributes.classList [ ( "selected", focusedIndex == index ) ]
                            , Html.Events.onFocus (FocusedInput index)
                            , Html.Events.onClick (FocusedInput index)
                            ]
                            [ Html.text
                                (char
                                    |> Maybe.map String.fromChar
                                    |> Maybe.withDefault ""
                                )
                            ]
                )
            |> Array.toList
        )


emptyRowView : Html Msg
emptyRowView =
    Html.div
        [ Html.Attributes.class "empty-row"
        ]
        (List.range 0 (defaultRowLength - 1) |> List.map (\_ -> Html.div [] []))



-- HELPERS


convertAttemptToString : Attempt -> String
convertAttemptToString attempt =
    List.foldr (String.cons << Tuple.first) "" attempt


focusCell : Int -> Cmd Msg
focusCell id =
    Task.attempt (\_ -> FocusedInput id) (Browser.Dom.focus ("cell" ++ String.fromInt id))


nextCell : Int -> Int
nextCell selected =
    min (selected + 1) (defaultRowLength - 1)


previousCell : Int -> Int
previousCell selected =
    max (selected - 1) 0


backgroundColor : LetterState -> String
backgroundColor state =
    case state of
        NotIncluded ->
            "rgb(180, 180, 180)"

        CorrectPlace ->
            "rgb(0, 190, 0)"

        IncorrectPlace ->
            "#ffb01e"


validateAttempt : String -> List Char -> Maybe (List Letter)
validateAttempt correct attempt =
    if not (FiveLetterWords.isValidWord (String.fromList attempt)) then
        Nothing

    else
        Just (compareWords correct attempt)


countByChar : String -> Dict Char Int
countByChar input =
    String.foldl
        (\char ->
            \acc ->
                Dict.update
                    char
                    (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)
                    acc
        )
        Dict.empty
        input


charRemains : Char -> Dict Char Int -> Bool
charRemains char dict =
    case Dict.get char dict of
        Just v ->
            v > 0

        Nothing ->
            False


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
                (countByChar correct)
                (List.map2 Tuple.pair (String.toList correct) attempt)

        decreaseCount : Char -> Dict Char Int -> Dict Char Int
        decreaseCount char dict =
            Dict.update
                char
                (Maybe.map (\v -> Basics.max 0 (v - 1)) >> Maybe.withDefault 0 >> Just)
                dict
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


type alias Cell =
    Maybe Char


cellHasChar : Cell -> Bool
cellHasChar c =
    case c of
        Just _ ->
            True

        Nothing ->
            False


canClearAttempt : List (Maybe Char) -> Bool
canClearAttempt =
    List.any cellHasChar


canSubmitAttempt : List (Maybe Char) -> Bool
canSubmitAttempt =
    List.all
        cellHasChar


createAlert : List Char -> Maybe String
createAlert attempt =
    let
        word =
            List.foldr
                String.cons
                ""
                attempt
    in
    Just (word ++ " is not a valid word!")


clearAlert : Cmd Msg
clearAlert =
    Process.sleep 1500 |> Task.perform (\_ -> AlertCleared)



-- PUBLIC FUNCTION


aCharCode : number
aCharCode =
    65


zCharCode : number
zCharCode =
    90


validChars : Set.Set Char
validChars =
    Set.fromList (List.range aCharCode zCharCode |> List.map Char.fromCode)


{-| Returns a set of possible words based on the users existing attempts
-}
matchingWords : List Attempt -> Set.Set String
matchingWords history =
    let
        -- Start with all characters allowed at every position
        initialAllowed : Array (Set.Set Char)
        initialAllowed =
            Array.repeat 5 validChars

        -- Update allowed letters & required letters based on guess feedback
        processLetter : Int -> Char -> LetterState -> ( Array (Set.Set Char), Set.Set Char ) -> ( Array (Set.Set Char), Set.Set Char )
        processLetter idx char state ( allowed, required ) =
            case state of
                CorrectPlace ->
                    ( Array.set idx (Set.singleton char) allowed
                    , Set.insert char required
                    )

                IncorrectPlace ->
                    ( Array.set idx (Set.remove char (Array.get idx allowed |> Maybe.withDefault Set.empty)) allowed
                    , Set.insert char required
                    )

                NotIncluded ->
                    if Set.member char required then
                        -- Gray but still needed elsewhere: ban only here
                        ( Array.set idx (Set.remove char (Array.get idx allowed |> Maybe.withDefault Set.empty)) allowed
                        , required
                        )

                    else
                        -- Truly absent: remove from all positions
                        ( Array.map (Set.remove char) allowed
                        , required
                        )

        processGuess : List ( Char, LetterState ) -> ( Array (Set.Set Char), Set.Set Char ) -> ( Array (Set.Set Char), Set.Set Char )
        processGuess guess acc =
            List.indexedMap Tuple.pair guess
                |> List.foldl (\( idx, ( char, state ) ) acc2 -> processLetter idx char state acc2) acc

        ( allowedAt, required_ ) =
            List.foldl processGuess ( initialAllowed, Set.empty ) history

        isValid : String -> Bool
        isValid word =
            let
                chars =
                    String.toList word
            in
            -- Fail fast: check required letters first
            Set.foldl
                (\c ok -> ok && String.contains (String.fromChar c) word)
                True
                required_
                && -- Then check per-position constraints
                   List.Extra.all2 Set.member chars (Array.toList allowedAt)
    in
    Set.filter isValid FiveLetterWords.words
