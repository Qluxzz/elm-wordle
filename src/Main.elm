module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (maxlength, style, value)
import Html.Events exposing (onInput)


type alias Model =
    { tries : Int
    , history : List String
    , currentAttempt : String
    }


type Msg
    = Increment
    | Decrement
    | CharEntered String


initalModel : Model
initalModel =
    { tries = 0
    , history =
        [ "testa"
        ]
    , currentAttempt = "andel"
    }


word : String
word =
    "spela"


view : Model -> Html Msg
view model =
    div
        [ style "padding" "10px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "10px"
        ]
        (List.append
            (List.map
                row
                model.history
            )
            [ activeRow model.currentAttempt ]
        )


rowBase : String -> (Char -> Html Msg) -> Html Msg
rowBase attempt elem =
    div
        [ style "display" "flex"
        , style "gap" "10px"
        ]
        (attempt
            |> String.toList
            |> List.map
                (\char ->
                    div
                        [ style "padding" "10px"
                        , style "border" "1px solid black"
                        , style "border-radius" "10px"
                        , style "text-transform" "uppercase"
                        , style "font-size" "32px"
                        ]
                        [ elem char ]
                )
        )


row : String -> Html Msg
row attempt =
    rowBase attempt (\char -> text (String.fromChar char))


activeRow : String -> Html Msg
activeRow attempt =
    rowBase attempt
        (\char ->
            input
                [ maxlength 1
                , style "width" "1em"
                , onInput (\str -> CharEntered (String.left 1 str))
                , value (String.fromChar char)
                ]
                []
        )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | tries = model.tries + 1 }

        Decrement ->
            { model | tries = model.tries - 1 }

        CharEntered char ->
            { model | currentAttempt = String.append model.currentAttempt char }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initalModel
        , view = view
        , update = update
        }
