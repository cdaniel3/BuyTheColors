module NoblesTest exposing (..)

import Expect 
import Nobles exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Nobles module"
        [ describe "init Nobles"
            [ test "10 total Nobles" <|
                \_ ->
                    let
                        noblesLength =
                            List.length Nobles.allNobles
                    in
                    Expect.equal 10 noblesLength
            , test "all have 3 VP" <|
                \_ ->
                    let
                        has3Points =
                            \noble ->
                                noble.victoryPoints == 3
                    in
                    List.all has3Points Nobles.allNobles
                        |> Expect.true "all nobles should have 3 points"
            ]
        ]
