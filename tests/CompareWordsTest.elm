module CompareWordsTest exposing (suite)

import Expect
import Game exposing (LetterState(..), compareWords)
import Test exposing (..)


cases : List ( ( String, String ), List ( Char, LetterState ) )
cases =
    [ ( ( "GUESS", "HUMAN" )
      , [ ( 'H', NotIncluded )
        , ( 'U', CorrectPlace )
        , ( 'M', NotIncluded )
        , ( 'A', NotIncluded )
        , ( 'N', NotIncluded )
        ]
      )
    , ( ( "ZOOMS", "GUESS" )
      , [ ( 'G', NotIncluded )
        , ( 'U', NotIncluded )
        , ( 'E', NotIncluded )
        , ( 'S', NotIncluded )
        , ( 'S', CorrectPlace )
        ]
      )
    , ( ( "GUESS", "SUCKS" )
      , [ ( 'S', IncorrectPlace )
        , ( 'U', CorrectPlace )
        , ( 'C', NotIncluded )
        , ( 'K', NotIncluded )
        , ( 'S', CorrectPlace )
        ]
      )
    , ( ( "STUFF", "SUCKS" )
      , [ ( 'S', CorrectPlace )
        , ( 'U', IncorrectPlace )
        , ( 'C', NotIncluded )
        , ( 'K', NotIncluded )
        , ( 'S', NotIncluded )
        ]
      )
    , ( ( "STUFF", "USSSS" )
      , [ ( 'U', IncorrectPlace )
        , ( 'S', IncorrectPlace )
        , ( 'S', NotIncluded )
        , ( 'S', NotIncluded )
        , ( 'S', NotIncluded )
        ]
      )
    ]


suite : Test
suite =
    describe "validateAttempt"
        (List.map
            (\( ( correct, attempt ), wanted ) ->
                test (correct ++ " " ++ attempt) <|
                    \_ ->
                        compareWords
                            correct
                            (attempt |> String.toList)
                            |> Expect.equal wanted
            )
            cases
        )
