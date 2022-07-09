module Main exposing (LetterState(..), main, validateAttempt)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import FiveLetterWords exposing (getRandomWord, isValidWord, wordsLength)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, disabled, id, maxlength, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
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
    = SubmitAttempt (List (Maybe Char))
    | CharEntered Int (Maybe Char)
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
    , currentAttempt = initRow
    , state = Loading
    }



{-
   alphabet : Dict Char LetterState
   alphabet =
       List.range 0 25 |> List.map (\i -> Char.fromCode (97 + i)) |> List.foldl (\letter -> \acc -> Dict.insert letter NotTried acc) Dict.empty
-}
-- CONSTANTS


defaultRowLength : Int
defaultRowLength =
    5


maxiumAttempts : Int
maxiumAttempts =
    6


initRow : Array (Maybe Char)
initRow =
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

                    Playing _ ->
                        activeRow model.currentAttempt

                    Loading ->
                        div [] [ text "Loading!" ]

                    Error message ->
                        div [] [ text message ]
               ]
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
                                    , id ("box" ++ String.fromInt index)
                                    , autocomplete False
                                    , onInput
                                        (\str ->
                                            str
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
        [ style "display" "flex"
        , style "gap" "10px"
        , onSubmit (SubmitAttempt (attempt |> Array.toList))
        ]
        (cells
            ++ [ button
                    [ disabled
                        (attempt
                            |> Array.toList
                            |> List.all
                                (\l ->
                                    case l of
                                        Just _ ->
                                            True

                                        Nothing ->
                                            False
                                )
                            |> not
                        )
                    , type_ "submit"
                    ]
                    [ text "Submit" ]
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
    focusInput "box0"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitAttempt [ Just a, Just b, Just c, Just d, Just e ] ->
            let
                word =
                    case model.state of
                        Playing w ->
                            w

                        Won w ->
                            w

                        Lost w ->
                            w

                        Error _ ->
                            Debug.todo "Should be impossible to reach"

                        Loading ->
                            Debug.todo "Should be impossible to reach"

                attempt =
                    [ a, b, c, d, e ]

                validatedAttempt =
                    validateAttempt word (Array.fromList attempt)
            in
            if isValidWord (attempt |> String.fromList) then
                ( { model
                    | history = model.history ++ [ validatedAttempt ]
                    , currentAttempt = initRow
                    , state =
                        if List.all (\( _, lS ) -> lS == CorrectPlace) validatedAttempt then
                            Won word

                        else if List.length model.history + 1 == maxiumAttempts then
                            Lost word

                        else
                            Playing word
                  }
                , focusFirstCell
                )

            else
                -- TODO: Show alert that word was invalid
                ( { model
                    | currentAttempt = initRow
                  }
                , focusFirstCell
                )

        SubmitAttempt _ ->
            ( model, Cmd.none )

        CharEntered index (Just char) ->
            -- TODO: Allow backspace to remove current char
            ( if char == ' ' then
                model

              else
                { model | currentAttempt = Array.set index (Just (Char.toLower char)) model.currentAttempt }
            , focusInput ("box" ++ String.fromInt (index + 1))
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


validateAttempt : String -> Array Char -> List Letter
validateAttempt correct attempt =
    -- Check first for correct place and remove these
    -- then check for incorrect place
    -- the rest are not included
    List.map2
        (\attemptChar ->
            \correctChar ->
                ( attemptChar
                , validateChar
                    attemptChar
                    correctChar
                    correct
                )
        )
        (Array.toList attempt)
        (String.toList correct)


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
    Browser.element
        { init = \_ -> ( initalModel, Random.generate GenerateRandomIndex (Random.int 0 wordsLength) )
        , view = view
        , update = \msg -> \model -> update msg model
        , subscriptions = \_ -> Sub.none
        }
