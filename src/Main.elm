module Main exposing (main)

import Array
import Browser
import Browser.Dom as Dom
import FiveLetterWords exposing (getRandomWord, isValidWord, wordsLength)
import Html exposing (..)
import Html.Attributes exposing (disabled, id, maxlength, style, type_, value)
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


type LetterState
    = CorrectPlace
    | IncorrectPlace
    | NotIncluded
    | NotTried


type alias Letter =
    ( Char, LetterState )


type Msg
    = SubmitAttempt
    | CharEntered (Maybe Char)
    | GenerateRandomIndex Int
    | NoOp


type alias Model =
    { history : List Attempt
    , currentAttempt : List Char
    , state : GameState
    }


initalModel : Model
initalModel =
    { history = []
    , currentAttempt = []
    , state = Loading
    }



{-
   alphabet : List Char
   alphabet =
       List.range 0 25 |> List.map (\i -> Char.fromCode (65 + i))
-}
-- CONSTANTS


defaultRowLength : Int
defaultRowLength =
    5


maxiumAttempts : Int
maxiumAttempts =
    6



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
                        div [ style "font-size" "32px" ] [ text "You won!" ]

                    Lost word ->
                        div [ style "font-size" "32px" ] [ text ("You lost! The word was " ++ String.toUpper word) ]

                    Playing _ ->
                        activeRow model.currentAttempt

                    Loading ->
                        div [] [ text "Loading!" ]
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


activeRow : List Char -> Html Msg
activeRow attempt =
    let
        arr =
            Array.fromList attempt
    in
    form
        [ style "display" "flex"
        , style "gap" "10px"
        , onSubmit SubmitAttempt
        ]
        ((List.range 0 (defaultRowLength - 1)
            |> List.map
                (\index ->
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
                                        |> CharEntered
                                )
                            , value
                                (case Array.get index arr of
                                    Just c ->
                                        String.fromChar c

                                    Nothing ->
                                        ""
                                )
                            ]
                            []
                        ]
                )
         )
            ++ [ button
                    [ onClick SubmitAttempt
                    , disabled (List.length attempt < defaultRowLength)
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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitAttempt ->
            let
                word =
                    case model.state of
                        Playing w ->
                            w

                        Won w ->
                            w

                        Lost w ->
                            w

                        Loading ->
                            Debug.todo "Should be impossible to reach"

                validatedAttempt =
                    validateAttempt word model.currentAttempt
            in
            if isValidWord (String.fromList model.currentAttempt) then
                ( { model
                    | history = model.history ++ [ validatedAttempt ]
                    , currentAttempt = []
                    , state =
                        if List.all (\( _, lS ) -> lS == CorrectPlace) validatedAttempt then
                            Won word

                        else if List.length model.history + 1 == maxiumAttempts then
                            Lost word

                        else
                            Playing word
                  }
                , focusInput "box0"
                )

            else
                -- TODO: Show alert that word was invalid
                ( { model
                    | currentAttempt = []
                  }
                , focusInput "box0"
                )

        CharEntered (Just char) ->
            -- TODO: Allow backspace and arbitrary navigation between cells
            ( if char == ' ' then
                model

              else
                { model | currentAttempt = model.currentAttempt ++ [ Char.toLower char ] }
            , focusInput ("box" ++ String.fromInt (List.length model.currentAttempt + 1))
            )

        CharEntered Nothing ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        GenerateRandomIndex index ->
            ( { model
                | state = Playing (getRandomWord index |> Maybe.withDefault "")
              }
            , Cmd.none
            )


validateAttempt : String -> List Char -> List Letter
validateAttempt correct attempt =
    List.map2 (\attemptChar -> \correctChar -> ( attemptChar, validateChar attemptChar correctChar correct )) attempt (String.toList correct)


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
