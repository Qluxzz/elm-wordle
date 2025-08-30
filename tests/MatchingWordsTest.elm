module MatchingWordsTest exposing (suite)

import Expect
import FiveLetterWords exposing (wordsLength)
import Game exposing (LetterState(..), matchingWords)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "matchingWords"
        [ test "empty params should return same amount as length of list" <|
            \_ -> matchingWords [] |> Set.size |> Expect.equal wordsLength
        , test "the order of the attempts doesn't change possible words" <|
            \_ ->
                let
                    attempts =
                        [ [ ( 'S', CorrectPlace ), ( 'L', CorrectPlace ), ( 'A', NotIncluded ), ( 'C', NotIncluded ), ( 'K', CorrectPlace ) ]
                        , [ ( 'H', NotIncluded ), ( 'E', IncorrectPlace ), ( 'L', IncorrectPlace ), ( 'P', NotIncluded ), ( 'S', IncorrectPlace ) ]
                        ]

                    attemptsReversed =
                        List.reverse attempts
                in
                matchingWords attempts |> Expect.equal (matchingWords attemptsReversed)
        ]
