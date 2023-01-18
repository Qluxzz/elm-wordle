module Main exposing (main)

import Browser
import FiveLetterWords exposing (getRandomWord, wordsLength)
import Game
import Html exposing (..)
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
        , subscriptions = \_ -> Sub.none
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
