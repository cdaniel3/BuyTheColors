module GemsTest exposing (..)

import Expect
import Gems exposing (Gems)
import Test exposing (..)


suite : Test
suite =
    describe "Gems module"
        [ describe "minus one gem"
            [ test "subtracts one from gem" <|
                \_ ->
                    let
                        oneRed =
                            Gems 1 0 0 0 0

                        empty =
                            Gems 0 0 0 0 0
                    in
                    Expect.equal empty (Gems.minusOneGem Gems.Red oneRed)
            ]
        , describe "isSubListOf"
            [ test "Given [Red] list A and [Red, Blue] list B, list A should be a subset of B" <|
                \_ ->
                    let
                        listA =
                            [ Gems.Red ]

                        listB =
                            [ Gems.Red, Gems.Blue ]
                    in
                    Gems.isSubListOf listA listB
                        |> Expect.true "A (Red) should be sublist of B (Red, Blue)"
            , test "Given [Red, Red] list A and [Red, Blue] list B, list A should be a subset of B" <|
                \_ ->
                    let
                        listA =
                            [ Gems.Red, Gems.Red ]

                        listB =
                            [ Gems.Red, Gems.Blue ]
                    in
                    Gems.isSubListOf listA listB
                        |> Expect.true "A (Red, Red) should be sublist of B (Red, Blue)"
            , test "Given [Green] list A and [Red, Blue] list B, list A should not be a subset of B" <|
                \_ ->
                    let
                        listA =
                            [ Gems.Green ]

                        listB =
                            [ Gems.Red, Gems.Blue ]
                    in
                    Gems.isSubListOf listA listB
                        |> Expect.false "List A (Green) should not be sublist of B (Red, Blue)"
            , test "Given [Red] list A and empty [] list B, list A should not be a subset of B" <|
                \_ ->
                    let
                        listA =
                            [ Gems.Red ]

                        listB =
                            []
                    in
                    Gems.isSubListOf listA listB
                        |> Expect.false "List A (Red) should not be sublist of empty List B"
            , test "Given empty [] list A and [Red, Blue] list B, list A should be a subset of B" <|
                \_ ->
                    let
                        listA =
                            []

                        listB =
                            [ Gems.Red, Gems.Blue ]
                    in
                    Gems.isSubListOf listA listB
                        |> Expect.true "List A (empty) should be sublist of List B (Red, Blue)"
            ]
        ]
