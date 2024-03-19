port module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import Browser.Navigation exposing (reload)
import FiveLetterWords exposing (getRandomWord, wordsLength)
import Game
import Html exposing (..)
import Html.Attributes as HA
import Html.Events
import Json.Decode as Decode
import Platform.Cmd as Cmd
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
    | GetLocalTime LocalTime
    | ShareResult -- Copies the result of the game to the clipboard to be shared
    | NoOp


type alias Model =
    { state : State
    , localTime : Maybe LocalTime
    , shared : Bool -- If the results of todays puzzle has been shared
    }


type alias LocalTime =
    { posix : Time.Posix, timeZone : Time.Zone }


port shareResult : String -> Cmd msg



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
                            , Html.button [ HA.id "share-button", Html.Events.onClick ShareResult ]
                                [ Html.text
                                    (if model.shared then
                                        "Shared!"

                                     else
                                        "Share!"
                                    )
                                ]
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
            let
                shouldRefresh =
                    Maybe.map (\localTime -> shouldRefreshBrowser localTime time) model.localTime
                        |> Maybe.withDefault False
            in
            ( { model | localTime = Maybe.map (\l -> { l | posix = time }) model.localTime }
            , if shouldRefresh then
                reload

              else
                Cmd.none
            )

        GetLocalTime timeAndZone ->
            ( { model | localTime = Just timeAndZone }, Cmd.none )

        ShareResult ->
            case ( model.state, model.localTime ) of
                ( Playing gameModel, Just localTime ) ->
                    let
                        formatted =
                            gameModel.history
                                |> List.map
                                    (\attempt ->
                                        List.map
                                            (\( _, state ) ->
                                                case state of
                                                    Game.CorrectPlace ->
                                                        "🟩"

                                                    Game.IncorrectPlace ->
                                                        "🟨"

                                                    Game.NotIncluded ->
                                                        "⬜"
                                            )
                                            attempt
                                    )
                                |> List.map (String.join "")
                                |> String.join "\n"

                        date =
                            toIsoDate localTime
                    in
                    ( { model | shared = True }, shareResult <| "Wordle " ++ date ++ "\n" ++ formatted )

                _ ->
                    ( model, Cmd.none )



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
            ( { state = Playing gameModel, localTime = Nothing, shared = False }
            , Cmd.batch
                [ Cmd.map Game cmd
                , Task.map2 LocalTime Time.now Time.here |> Task.perform GetLocalTime
                ]
            )

        Nothing ->
            ( { state = Error "Failed to get random word", localTime = Nothing, shared = False }, Cmd.none )


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
timeUntilMidnight { posix, timeZone } =
    let
        hours =
            23 - Time.toHour timeZone posix

        minutes =
            59 - Time.toMinute timeZone posix

        seconds =
            59 - Time.toSecond timeZone posix
    in
    [ hours, minutes, seconds ]
        |> List.map padWithZero
        |> String.join ":"


shouldRefreshBrowser : LocalTime -> Time.Posix -> Bool
shouldRefreshBrowser before after =
    let
        day =
            Time.toDay before.timeZone
    in
    day after > day before.posix


padWithZero : Int -> String
padWithZero =
    String.fromInt >> String.padLeft 2 '0'


toIsoDate : LocalTime -> String
toIsoDate { posix, timeZone } =
    let
        year =
            Time.toYear timeZone posix

        month =
            case Time.toMonth timeZone posix of
                Time.Jan ->
                    1

                Time.Feb ->
                    2

                Time.Mar ->
                    3

                Time.Apr ->
                    4

                Time.May ->
                    5

                Time.Jun ->
                    6

                Time.Jul ->
                    7

                Time.Aug ->
                    8

                Time.Sep ->
                    9

                Time.Oct ->
                    10

                Time.Nov ->
                    11

                Time.Dec ->
                    12

        date =
            Time.toDay timeZone posix
    in
    String.fromInt year ++ "-" ++ padWithZero month ++ "-" ++ padWithZero date
