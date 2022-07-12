module ValidateAttempt exposing (..)

import Expect
import Main exposing (LetterState(..), validateAttempt)
import Test exposing (..)


suite : Test
suite =
    describe "validateAttempt"
        [ test "valid result" <|
            \_ ->
                validateAttempt "guess" ("human" |> String.toList)
                    |> Expect.equal
                        (Just
                            [ ( 'h', NotIncluded )
                            , ( 'u', CorrectPlace )
                            , ( 'm', NotIncluded )
                            , ( 'a', NotIncluded )
                            , ( 'n', NotIncluded )
                            ]
                        )
        , test "multiple of same letter, if all instances of the correct char has been placed, should say NotIncluded for rest" <|
            \_ ->
                validateAttempt "guess" ("sucks" |> String.toList)
                    |> Expect.equal
                        (Just
                            [ ( 's', IncorrectPlace )
                            , ( 'u', CorrectPlace )
                            , ( 'c', NotIncluded )
                            , ( 'k', NotIncluded )
                            , ( 's', CorrectPlace )
                            ]
                        )
        , test "multiple of same letter, if all instances of the correct char has been placed, should say NotInluded for rest" <|
            \_ ->
                validateAttempt "stuff" ("sucks" |> String.toList)
                    |> Expect.equal
                        (Just
                            [ ( 's', CorrectPlace )
                            , ( 'u', IncorrectPlace )
                            , ( 'c', NotIncluded )
                            , ( 'k', NotIncluded )
                            , ( 's', NotIncluded )
                            ]
                        )
        ]
