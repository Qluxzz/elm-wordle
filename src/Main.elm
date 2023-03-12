module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import FiveLetterWords exposing (getRandomWord, wordsLength)
import Game
import Html exposing (..)
import Html.Attributes as HA
import Json.Decode as Decode
import Random
import Task
import Time



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
    | Tick Time.Posix
    | LocalTime LocalTime
    | NoOp


type alias Model =
    { state : State
    , localTime : Maybe LocalTime
    }


type alias LocalTime =
    ( Time.Posix, Time.Zone )



-- VIEW


view : Model -> List (Html Msg)
view model =
    case model.state of
        Playing gameModel ->
            [ Html.map Game (Game.view gameModel)
            , if List.member gameModel.state [ Game.Won, Game.Lost ] then
                let
                    resultsView =
                        gameResultsView
                            gameModel.correctWord
                            [ model.localTime
                                |> Maybe.map timeUntilMidnightView
                                |> Maybe.withDefault (Html.text "")
                            ]
                in
                case gameModel.state of
                    Game.Won ->
                        resultsView "You won!"

                    Game.Lost ->
                        resultsView ("You lost! The correct word was " ++ gameModel.correctWord)

                    _ ->
                        Html.text ""

              else
                Html.text ""
            ]

        Error message ->
            [ div [] [ text message ]
            ]


timeUntilMidnightView : LocalTime -> Html msg
timeUntilMidnightView localTime =
    Html.text ("Next word in: " ++ timeUntilMidnight localTime)


gameResultsView : String -> List (Html Msg) -> String -> Html Msg
gameResultsView correctWord extra resultText =
    div [ HA.class "overlay" ]
        ([ h1 [ HA.style "text-align" "center" ]
            [ text resultText ]
         , a
            [ HA.href ("https://www.merriam-webster.com/dictionary/" ++ String.toLower correctWord)
            , HA.target "_blank"
            ]
            [ text ("Dictionary entry for \"" ++ String.toLower correctWord ++ "\"") ]
         ]
            ++ extra
        )



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

        Tick time ->
            ( { model | localTime = Maybe.map (Tuple.mapFirst (\_ -> time)) model.localTime }, Cmd.none )

        LocalTime timeAndZone ->
            ( { model | localTime = Just timeAndZone }, Cmd.none )



-- MAIN


type alias Flags =
    { seed : Int
    , save : Maybe (List String)
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = \{ seed, save } -> startNewGame seed save
        , view =
            \model ->
                { title = "ELM Wordle"
                , body = view model
                }
        , update = \msg -> \model -> update msg model
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onKeyUp keyDecoder
                    , Time.every 1000 Tick
                    ]
        }



-- Helpers


startNewGame : Int -> Maybe Game.SavedState -> ( Model, Cmd Msg )
startNewGame seed savedState =
    let
        ( index, _ ) =
            Random.step (Random.int 0 wordsLength) (Random.initialSeed seed)
    in
    case getRandomWord index of
        Just word ->
            let
                ( gameModel, cmd ) =
                    Game.init word savedState
            in
            ( { state = Playing gameModel, localTime = Nothing }
            , Cmd.batch
                [ Cmd.map Game cmd
                , Task.map2 Tuple.pair Time.now Time.here |> Task.perform LocalTime
                ]
            )

        Nothing ->
            ( { state = Error "Failed to get random word", localTime = Nothing }, Cmd.none )


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

                space =
                    32
            in
            if code == space then
                Game Game.FocusNextCell

            else if between a z code then
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


timeUntilMidnight : LocalTime -> String
timeUntilMidnight ( now, zone ) =
    let
        hours =
            23 - Time.toHour zone now

        minutes =
            59 - Time.toMinute zone now

        seconds =
            59 - Time.toSecond zone now
    in
    [ hours, minutes, seconds ]
        |> List.map (String.fromInt >> String.padLeft 2 '0')
        |> String.join ":"
