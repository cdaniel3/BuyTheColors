module PlayerTest exposing (..)

import Cards exposing (Card, Noble)
import Nobles exposing (noble3R3B3G3Points, noble4B4G3Points)
import Expect
import Gems exposing (..)
import Player exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Player module"
        [ describe "nobleVisit"
            [ test "no nobles visit" <|
                \_ ->
                    let
                        easyNoble =
                            let
                                visitAmount =
                                    initEmptyGems
                                        |> setGemAmount Red 3
                            in
                            Noble visitAmount 3 "url"

                        playerWithNoCards =
                            initPlayer

                        ( visiting, _ ) =
                            nobleVisit [ easyNoble ] playerWithNoCards
                    in
                    Expect.equal 0 (List.length visiting)
            , test "noble does not visit player with too few discounts" <|
                \_ ->
                    let
                        nobleRequiring3Red =
                            let
                                visitAmount =
                                    initEmptyGems
                                        |> setGemAmount Red 3
                            in
                            Noble visitAmount 1 "url"

                        playerWithOneRedDiscount =
                            { initPlayer
                                | cards =
                                    [ Card (Gems 0 0 0 4 0) Red 1 "" Cards.Level1 ]
                            }

                        ( visiting, _) =
                            nobleVisit [ nobleRequiring3Red ] playerWithOneRedDiscount
                    in
                    Expect.equal 0 (List.length visiting)
            , test "noble visits player with exact discount amount" <|
                \_ ->
                    let
                        nobleRequiring3Red =
                            let
                                visitAmount =
                                    initEmptyGems
                                        |> setGemAmount Red 3
                            in
                            Noble visitAmount 1 "url"

                        playerWithThreeRedDiscounts =
                            { initPlayer
                                | cards =
                                    [ Card (Gems 0 0 0 4 0) Red 1 "" Cards.Level1
                                    , Card (Gems 0 0 0 1 0) Red 0 "" Cards.Level1
                                    , Card (Gems 0 2 4 0 3) Red 2 "" Cards.Level2
                                    ]
                            }

                        ( visiting, _ ) =
                            nobleVisit [ nobleRequiring3Red ] playerWithThreeRedDiscounts
                    in
                    Expect.equal 1 (List.length visiting)
            , test "noble from actual game does not visit player with too few discounts" <|
                \_ ->
                    let
                        playerWithOneRedDiscount =
                            { initPlayer
                                | cards =
                                    [ Card (Gems 0 0 0 4 0) Red 1 "" Cards.Level1 ]
                            }

                        ( visiting, _ ) =
                            nobleVisit [ noble3R3B3G3Points ] playerWithOneRedDiscount
                    in
                    Expect.equal 0 (List.length visiting)
            , test "noble from actual game visits player with exact discount amount" <|
                \_ ->
                    let
                        playerWith3Red3Blue3GreenDiscounts =
                            { initPlayer
                                | cards =
                                    [ Card (Gems 0 0 0 4 0) Red 1 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Red 1 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Red 1 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Blue 1 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Blue 1 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Blue 1 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Green 1 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Green 1 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Green 1 "" Cards.Level1
                                    ]

                                {-
                                   [ initFrom "Level1Deck" "00040_1_Red_1"
                                   , initFrom "Level1Deck" "00040_1_Red_1"
                                   , initFrom "Level1Deck" "00040_1_Red_1"
                                   , initFrom "Level1Deck" "00040_1_Blue_1"
                                   , initFrom "Level1Deck" "00040_1_Blue_1"
                                   , initFrom "Level1Deck" "00040_1_Blue_1"
                                   , initFrom "Level1Deck" "00040_1_Green_1"
                                   , initFrom "Level1Deck" "00040_1_Green_1"
                                   , initFrom "Level1Deck" "00040_1_Green_1"
                                   ]
                                -}
                            }

                        ( visiting, _ ) =
                            nobleVisit [ noble3R3B3G3Points ] playerWith3Red3Blue3GreenDiscounts
                    in
                    Expect.equal 1 (List.length visiting)
            , test "noble from actual game visits player from actual game" <|
                \_ ->
                    let
                        playerFromGame =
                            { initPlayer
                                | cards =
                                    [ Card (Gems 0 0 0 4 0) White 0 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) White 0 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Blue 0 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Blue 0 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Green 0 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Red 0 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Red 0 "" Cards.Level1
                                    , Card (Gems 0 0 0 4 0) Black 0 "" Cards.Level1
                                    ]

                                {-
                                   [ initFrom "Level1Deck" "00040_0_White_1"
                                   , initFrom "Level1Deck" "00040_0_White_1"
                                   , initFrom "Level1Deck" "00040_0_Blue_1"
                                   , initFrom "Level1Deck" "00040_0_Blue_1"
                                   , initFrom "Level1Deck" "00040_0_Green_1"
                                   , initFrom "Level1Deck" "00040_0_Red_1"
                                   , initFrom "Level1Deck" "00040_0_Red_1"
                                   , initFrom "Level1Deck" "00040_0_Black_1"
                                   ]
                                -}
                            }

                        ( visiting, _ ) =
                            nobleVisit [ noble4B4G3Points ] playerFromGame
                    in
                    Expect.equal 0 (List.length visiting)
            ]
        , describe "isVisiting"
            [ test "Not enough gems for required visit amount" <|
                \_ ->
                    let
                        discounts =
                            [ White, White, Blue, Blue, Green, Red, Red, Black ]

                        visitAmount =
                            Gems.initEmptyGems
                                |> Gems.setGemAmount Blue 4
                                |> Gems.setGemAmount Green 4
                    in
                    Expect.false "isVisiting should be false" (isVisiting discounts visitAmount)
            ]
        ]
