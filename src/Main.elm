module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import FiveLetterWords exposing (getRandomWord, wordsLength)
import Game
import Html exposing (..)
import Json.Decode as Decode
import Random



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


type State
    = Playing Game.Model
    | Error String


type Msg
    = Game Game.Msg
    | NoOp


type alias Model =
    { state : State
    }



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ case model.state of
        Playing gameModel ->
            Html.map Game (Game.view gameModel)

        Error message ->
            div [] [ text message ]
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Game gameMsg ->
            case model.state of
                Playing gameModel ->
                    let
                        ( updatedModel, cmd ) =
                            Game.update gameMsg gameModel
                    in
                    ( { model | state = Playing updatedModel }, Cmd.map Game cmd )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- MAIN


main : Program Int Model Msg
main =
    Browser.document
        { init = \seed -> startNewGame seed
        , view =
            \model ->
                { title = "ELM Wordle"
                , body = view model
                }
        , update = \msg -> \model -> update msg model
        , subscriptions = \_ -> onKeyUp keyDecoder
        }



-- Helpers


startNewGame : Int -> ( Model, Cmd Msg )
startNewGame seed =
    let
        ( index, _ ) =
            Random.step (Random.int 0 wordsLength) (Random.initialSeed seed)
    in
    case getRandomWord index of
        Just word ->
            let
                ( gameModel, cmd ) =
                    Game.init word
            in
            ( { state = Playing gameModel }, Cmd.map Game cmd )

        Nothing ->
            ( { state = Error "Failed to get random word" }, Cmd.none )


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


between : comparable -> comparable -> comparable -> Bool
between min max value =
    value >= min && value <= max


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            let
                normalizedChar =
                    Char.toUpper char

                code =
                    Char.toCode normalizedChar

                a =
                    65

                z =
                    90
            in
            if between a z code then
                Game (Game.CharEntered normalizedChar)

            else
                NoOp

        _ ->
            case keyValue of
                "Backspace" ->
                    Game Game.RemoveChar

                "Enter" ->
                    Game Game.SubmitAttempt

                "ArrowLeft" ->
                    Game Game.FocusPreviousCell

                "ArrowRight" ->
                    Game Game.FocusNextCell

                _ ->
                    NoOp
