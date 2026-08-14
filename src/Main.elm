port module Main exposing (init, main, update, view)

import Browser
import Browser.Events
import Browser.Navigation
import FiveLetterWords
import Game
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Random
import Settings exposing (Settings)
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
      -- Settings
    | ShowSettings
    | HideSettings
    | ToggleClearInvalidAttempt Bool


type alias Model =
    { state : State
    , localTime : Maybe LocalTime
    , shared : Bool -- If the results of todays puzzle has been shared
    , showSettings : Bool
    , settings : Settings
    }


type alias LocalTime =
    { posix : Time.Posix, timeZone : Time.Zone }


port shareResult : String -> Cmd msg


port persistSettings : Settings -> Cmd msg


type Effect
    = None
    | MapGame Game.Effect
    | FetchLocalTime
    | CopyResult String
    | PersistSettings Settings
    | Reload
    | Batch (List Effect)


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        None ->
            Cmd.none

        MapGame gameEffect ->
            Cmd.map Game (Game.perform gameEffect)

        FetchLocalTime ->
            Task.map2 LocalTime Time.now Time.here |> Task.perform GetLocalTime

        CopyResult result ->
            shareResult result

        PersistSettings settings ->
            persistSettings settings

        Reload ->
            Browser.Navigation.reload

        Batch effects ->
            effects |> List.map perform |> Cmd.batch



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "ELM Wordle"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    headerView
        :: (case model.state of
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
                                    , Html.button [ Html.Attributes.id "share-button", Html.Events.onClick ShareResult ]
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
                    [ Html.div [] [ Html.text message ] ]
           )
        ++ [ if model.showSettings then
                settingsView model.settings

             else
                Html.text ""
           ]


timeUntilMidnightView : LocalTime -> Html msg
timeUntilMidnightView localTime =
    Html.text ("Next word in: " ++ timeUntilMidnight localTime)


gameResultsView : String -> List (Html Msg) -> String -> Html Msg
gameResultsView correctWord extra resultText =
    Html.div [ Html.Attributes.class "overlay" ]
        ([ Html.h1 [ Html.Attributes.style "text-align" "center" ]
            [ Html.text resultText ]
         , Html.a
            [ Html.Attributes.href ("https://www.merriam-webster.com/dictionary/" ++ String.toLower correctWord)
            , Html.Attributes.target "_blank"
            ]
            [ Html.text ("Dictionary entry for \"" ++ String.toLower correctWord ++ "\"") ]
         ]
            ++ extra
        )


headerView : Html Msg
headerView =
    Html.header []
        [ Html.h1 [] [ Html.text "Elm Wordle" ]
        , Html.button [ Html.Events.onClick ShowSettings ] [ Html.text "⚙️" ]
        ]


settingsView : Settings -> Html Msg
settingsView settings =
    Html.div [ Html.Attributes.id "settings" ]
        [ Html.h2 [] [ Html.text "Settings" ]
        , Html.p [] [ Html.text "Clear letters after invalid attempt?", Html.input [ Html.Events.onCheck ToggleClearInvalidAttempt, Html.Attributes.type_ "checkbox", Html.Attributes.style "margin-left" "10px", Html.Attributes.checked settings.clearInvalidAttempt ] [] ]
        , Html.button [ Html.Events.onClick HideSettings ] [ Html.text "Close" ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        Game gameMsg ->
            case model.state of
                Playing gameModel ->
                    let
                        ( updatedModel, effect ) =
                            Game.update gameMsg gameModel model.settings
                    in
                    ( { model | state = Playing updatedModel }, MapGame effect )

                _ ->
                    ( model, None )

        NoOp ->
            ( model, None )

        Tick time ->
            let
                shouldRefresh =
                    Maybe.map (\localTime -> shouldRefreshBrowser localTime time) model.localTime
                        |> Maybe.withDefault False
            in
            ( { model | localTime = Maybe.map (\l -> { l | posix = time }) model.localTime }
            , if shouldRefresh then
                Reload

              else
                None
            )

        GetLocalTime timeAndZone ->
            ( { model | localTime = Just timeAndZone }, None )

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
                    ( { model | shared = True }, CopyResult <| "Wordle " ++ date ++ "\n" ++ formatted )

                _ ->
                    ( model, None )

        ShowSettings ->
            ( { model | showSettings = True }, None )

        HideSettings ->
            ( { model | showSettings = False }, None )

        ToggleClearInvalidAttempt value ->
            let
                settings =
                    model.settings

                updatedSettings =
                    { settings | clearInvalidAttempt = value }
            in
            ( { model | settings = updatedSettings }, PersistSettings updatedSettings )



-- MAIN


type alias Flags =
    { seed : Int
    , save : Maybe (List String)
    , settings : Maybe Settings
    }


defaultSettings : Settings
defaultSettings =
    { clearInvalidAttempt = False
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init >> Tuple.mapSecond perform
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond perform
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onKeyUp keyDecoder
                    , Time.every 1000 Tick
                    ]
        }



-- Helpers


init : Flags -> ( Model, Effect )
init flags =
    startNewGame flags


startNewGame : Flags -> ( Model, Effect )
startNewGame { seed, save, settings } =
    let
        ( index, _ ) =
            Random.step (Random.int 0 FiveLetterWords.wordsLength) (Random.initialSeed seed)

        initSettings =
            settings |> Maybe.withDefault defaultSettings

        defaultModel : Model
        defaultModel =
            { state = Error "Failed to get random word"
            , localTime = Nothing
            , shared = False
            , showSettings = False
            , settings = initSettings
            }
    in
    case FiveLetterWords.getRandomWord index of
        Just word ->
            let
                ( gameModel, effect ) =
                    Game.init word save
            in
            ( { defaultModel | state = Playing gameModel }
            , Batch [ MapGame effect, FetchLocalTime ]
            )

        Nothing ->
            ( defaultModel, None )


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map toKey (Json.Decode.field "key" Json.Decode.string)


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
