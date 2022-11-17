module CompareWordsTest exposing (suite)

import Expect
import Game exposing (LetterState(..), compareWords)
import Test exposing (..)


suite : Test
suite =
    describe "validateAttempt"
        [ test "valid result" <|
            \_ ->
                compareWords "GUESS" ("HUMAN" |> String.toList)
                    |> Expect.equal
                        [ ( 'H', NotIncluded )
                        , ( 'U', CorrectPlace )
                        , ( 'M', NotIncluded )
                        , ( 'A', NotIncluded )
                        , ( 'N', NotIncluded )
                        ]
        , test "another valid result" <|
            \_ ->
                compareWords "ZOOMS" ("GUESS" |> String.toList)
                    |> Expect.equal
                        [ ( 'G', NotIncluded )
                        , ( 'U', NotIncluded )
                        , ( 'E', NotIncluded )
                        , ( 'S', NotIncluded )
                        , ( 'S', CorrectPlace )
                        ]
        , describe "multiple of same letter"
            [ test "one correct place and one incorrect place" <|
                \_ ->
                    compareWords "GUESS" ("SUCKS" |> String.toList)
                        |> Expect.equal
                            [ ( 'S', IncorrectPlace )
                            , ( 'U', CorrectPlace )
                            , ( 'C', NotIncluded )
                            , ( 'K', NotIncluded )
                            , ( 'S', CorrectPlace )
                            ]
            , test "one correct place and one not included" <|
                \_ ->
                    compareWords "STUFF" ("SUCKS" |> String.toList)
                        |> Expect.equal
                            [ ( 'S', CorrectPlace )
                            , ( 'U', IncorrectPlace )
                            , ( 'C', NotIncluded )
                            , ( 'K', NotIncluded )
                            , ( 'S', NotIncluded )
                            ]
            , test "one incorrect place and rest not included" <|
                \_ ->
                    compareWords "STUFF" ("USSSS" |> String.toList)
                        |> Expect.equal
                            [ ( 'U', IncorrectPlace )
                            , ( 'S', IncorrectPlace )
                            , ( 'S', NotIncluded )
                            , ( 'S', NotIncluded )
                            , ( 'S', NotIncluded )
                            ]
            ]
        ]
