module CompareWordsTest exposing (suite)

import Expect
import Game
import Test exposing (Test)


cases : List ( ( String, String ), List ( Char, Game.LetterState ) )
cases =
    [ ( ( "GUESS", "HUMAN" )
      , [ ( 'H', Game.NotIncluded )
        , ( 'U', Game.CorrectPlace )
        , ( 'M', Game.NotIncluded )
        , ( 'A', Game.NotIncluded )
        , ( 'N', Game.NotIncluded )
        ]
      )
    , ( ( "ZOOMS", "GUESS" )
      , [ ( 'G', Game.NotIncluded )
        , ( 'U', Game.NotIncluded )
        , ( 'E', Game.NotIncluded )
        , ( 'S', Game.NotIncluded )
        , ( 'S', Game.CorrectPlace )
        ]
      )
    , ( ( "GUESS", "SUCKS" )
      , [ ( 'S', Game.IncorrectPlace )
        , ( 'U', Game.CorrectPlace )
        , ( 'C', Game.NotIncluded )
        , ( 'K', Game.NotIncluded )
        , ( 'S', Game.CorrectPlace )
        ]
      )
    , ( ( "STUFF", "SUCKS" )
      , [ ( 'S', Game.CorrectPlace )
        , ( 'U', Game.IncorrectPlace )
        , ( 'C', Game.NotIncluded )
        , ( 'K', Game.NotIncluded )
        , ( 'S', Game.NotIncluded )
        ]
      )
    , ( ( "STUFF", "USSSS" )
      , [ ( 'U', Game.IncorrectPlace )
        , ( 'S', Game.IncorrectPlace )
        , ( 'S', Game.NotIncluded )
        , ( 'S', Game.NotIncluded )
        , ( 'S', Game.NotIncluded )
        ]
      )
    ]


suite : Test
suite =
    Test.describe "validateAttempt"
        (List.map
            (\( ( correct, attempt ), wanted ) ->
                Test.test (correct ++ " " ++ attempt) <|
                    \_ ->
                        Game.compareWords
                            correct
                            (attempt |> String.toList)
                            |> Expect.equal wanted
            )
            cases
        )
