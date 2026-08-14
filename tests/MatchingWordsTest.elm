module MatchingWordsTest exposing (suite)

import Expect
import FiveLetterWords
import Game
import Set
import Test exposing (Test)


suite : Test
suite =
    Test.describe "matchingWords"
        [ Test.test "empty params should return same amount as length of list" <|
            \_ -> Game.matchingWords [] |> Set.size |> Expect.equal FiveLetterWords.wordsLength
        , Test.test "the order of the attempts doesn't change possible words" <|
            \_ ->
                let
                    attempts =
                        [ [ ( 'S', Game.CorrectPlace ), ( 'L', Game.CorrectPlace ), ( 'A', Game.NotIncluded ), ( 'C', Game.NotIncluded ), ( 'K', Game.CorrectPlace ) ]
                        , [ ( 'H', Game.NotIncluded ), ( 'E', Game.IncorrectPlace ), ( 'L', Game.IncorrectPlace ), ( 'P', Game.NotIncluded ), ( 'S', Game.IncorrectPlace ) ]
                        ]

                    attemptsReversed =
                        List.reverse attempts
                in
                Game.matchingWords attempts |> Expect.equal (Game.matchingWords attemptsReversed)
        ]
