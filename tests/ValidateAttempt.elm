module ValidateAttempt exposing (..)

import Expect
import Main exposing (LetterState(..), compareWords)
import Test exposing (..)


suite : Test
suite =
    describe "validateAttempt"
        [ test "valid result" <|
            \_ ->
                compareWords "guess" ("human" |> String.toList)
                    |> Expect.equal
                        [ ( 'h', NotIncluded )
                        , ( 'u', CorrectPlace )
                        , ( 'm', NotIncluded )
                        , ( 'a', NotIncluded )
                        , ( 'n', NotIncluded )
                        ]
        , describe "multiple of same letter"
            [ test "one correct place and one incorrect place" <|
                \_ ->
                    compareWords "guess" ("sucks" |> String.toList)
                        |> Expect.equal
                            [ ( 's', IncorrectPlace )
                            , ( 'u', CorrectPlace )
                            , ( 'c', NotIncluded )
                            , ( 'k', NotIncluded )
                            , ( 's', CorrectPlace )
                            ]
            , test "one correct place and one not included" <|
                \_ ->
                    compareWords "stuff" ("sucks" |> String.toList)
                        |> Expect.equal
                            [ ( 's', CorrectPlace )
                            , ( 'u', IncorrectPlace )
                            , ( 'c', NotIncluded )
                            , ( 'k', NotIncluded )
                            , ( 's', NotIncluded )
                            ]
            , test "one incorrect place and rest not included" <|
                \_ ->
                    compareWords "stuff" ("ussss" |> String.toList)
                        |> Expect.equal
                            [ ( 'u', IncorrectPlace )
                            , ( 's', IncorrectPlace )
                            , ( 's', NotIncluded )
                            , ( 's', NotIncluded )
                            , ( 's', NotIncluded )
                            ]
            ]
        ]
