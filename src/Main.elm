module Main exposing (main)

import Array
import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (disabled, id, maxlength, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
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


type alias Model =
    { history : List Attempt
    , currentAttempt : List Char
    }


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
    | NoOp


initalModel : Model
initalModel =
    { history = []
    , currentAttempt = []
    }



{-
   alphabet : List Char
   alphabet =
       List.range 0 25 |> List.map (\i -> Char.fromCode (65 + i))
-}


word : String
word =
    "GUESS"


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
            ++ [ activeRow model.currentAttempt
               ]
        )


defaultRowLength : Int
defaultRowLength =
    5


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
                        , style "text-transform" "uppercase"
                        , style "font-size" "32px"
                        , style "font-weight" "bold"
                        , style "text-align" "center"
                        , style "width" "1em"
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitAttempt ->
            ( { model
                | history = model.history ++ [ validateAttempt model.currentAttempt ]
                , currentAttempt = []
              }
            , focusInput "box0"
            )

        CharEntered (Just char) ->
            ( if char == ' ' then
                model

              else
                { model | currentAttempt = model.currentAttempt ++ [ Char.toUpper char ] }
            , focusInput ("box" ++ String.fromInt (List.length model.currentAttempt + 1))
            )

        CharEntered Nothing ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


validateAttempt_ : String -> List Char -> List Letter
validateAttempt_ correct attempt =
    List.map2 (\attemptChar -> \correctChar -> ( attemptChar, validateChar attemptChar correctChar correct )) attempt (String.toList correct)


validateAttempt : List Char -> List Letter
validateAttempt =
    validateAttempt_ word


validateChar : Char -> Char -> String -> LetterState
validateChar attemptChar correctChar correct =
    if attemptChar == correctChar then
        CorrectPlace

    else if String.contains (String.fromChar attemptChar) correct then
        IncorrectPlace

    else
        NotIncluded


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initalModel, Cmd.none )
        , view = view
        , update = \msg -> \model -> update msg model
        , subscriptions = \_ -> Sub.none
        }
