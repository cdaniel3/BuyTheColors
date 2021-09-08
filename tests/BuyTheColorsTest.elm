module BuyTheColorsTest exposing (..)

import Cards exposing (..)
import Expect
import Gems exposing (..)
import Player exposing (..)
import BuyTheColors exposing (DiscardGemsState, TableGems)
import Test exposing (..)



-- import Cards exposing (Noble, initFrom, noble3R3B3G3Points, noble4B4G3Points)
-- import Fuzz exposing (Fuzzer, int, list, string)
-- import Player exposing (..)


suite : Test
suite =
    describe "Splendor module"
        [ describe "initGameState"
            [ test "Table gems should have 4 each for 2 players" <|
                \_ ->
                    let
                        gameState =
                            BuyTheColors.initGameState [ "player1", "player2" ]

                        gems =
                            gameState.tableGems
                    in
                    Gems.getGemAmount Red gems
                        == 4
                        && Gems.getGemAmount Blue gems
                        == 4
                        && Gems.getGemAmount Green gems
                        == 4
                        && Gems.getGemAmount White gems
                        == 4
                        && Gems.getGemAmount Black gems
                        == 4
                        |> Expect.true "table gems has 4 gems each for 2p"
            , test "Table gems should have 5 each for 3 players" <|
                \_ ->
                    let
                        gameState =
                            BuyTheColors.initGameState [ "player1", "player2", "player3" ]

                        gems =
                            gameState.tableGems
                    in
                    Gems.getGemAmount Red gems
                        == 5
                        && Gems.getGemAmount Blue gems
                        == 5
                        && Gems.getGemAmount Green gems
                        == 5
                        && Gems.getGemAmount White gems
                        == 5
                        && Gems.getGemAmount Black gems
                        == 5
                        |> Expect.true "table gems has 5 gems each for 3p"
            , test "Table gems should have 7 each for 4 players" <|
                \_ ->
                    let
                        gameState =
                            BuyTheColors.initGameState [ "player1", "player2", "player3", "player4" ]

                        gems =
                            gameState.tableGems
                    in
                    Gems.getGemAmount Red gems
                        == 7
                        && Gems.getGemAmount Blue gems
                        == 7
                        && Gems.getGemAmount Green gems
                        == 7
                        && Gems.getGemAmount White gems
                        == 7
                        && Gems.getGemAmount Black gems
                        == 7
                        |> Expect.true "table gems has 7 gems each for 4p"
            ]
        , describe "taking gems"
            [ test "Taking 3 gems of different colors should result in 3 diff color action type" <|
                \_ ->
                    let
                        gemsToTake =
                            [ Red, Blue, Green ]

                        tableGems =
                            Gems 1 1 1 0 0
                    in
                    BuyTheColors.checkGrabGemType gemsToTake tableGems
                        |> Expect.equal (Just BuyTheColors.ThreeOfDifferentColor)
            , test "Taking 1 blue, 1 red should result in remaining" <|
                \_ ->
                    let
                        gemsToTake =
                            [ Red, Blue ]

                        tableGems =
                            Gems 1 1 0 0 0
                    in
                    BuyTheColors.checkGrabGemType gemsToTake tableGems
                        |> Expect.equal (Just BuyTheColors.Available)
            , test "Taking 1 blue when 3 blue are left" <|
                \_ ->
                    let
                        gemsToTake =
                            [ Blue ]

                        tableGems =
                            Gems 0 3 0 0 0
                    in
                    BuyTheColors.checkGrabGemType gemsToTake tableGems
                        |> Expect.equal (Just BuyTheColors.Available)
            , test "Taking 1 blue, 1 red when 1 blue, 2 red are left" <|
                \_ ->
                    let
                        gemsToTake =
                            [ Blue, Red ]

                        tableGems =
                            Gems 2 1 0 0 0
                    in
                    BuyTheColors.checkGrabGemType gemsToTake tableGems
                        |> Expect.equal (Just BuyTheColors.Available)
            ]
        , describe "discarding gems"
            [ test "Discarding 1 gem should add 1 to discarded gems and subtract 1 to kept gems" <|
                \_ ->
                    let
                        -- Player has 1 red and 1 blue
                        playerGems =
                            Gems 1 1 0 0 0

                        -- initializing empty discardGemsState
                        discardGemsState =
                            DiscardGemsState playerGems (Gems 0 0 0 0 0) 0

                        keepingOneRed =
                            Gems 1 0 0 0 0

                        discardingOneBlue =
                            Gems 0 1 0 0 0
                    in
                    BuyTheColors.discardGem Blue discardGemsState
                        |> Expect.equal (DiscardGemsState keepingOneRed discardingOneBlue 0)
            , test "Discarding 1 white gem when player has white and black gems" <|
                \_ ->
                    let
                        playerGems =
                            Gems 0 0 0 1 1

                        discardGemsState =
                            DiscardGemsState playerGems (Gems 0 0 0 0 0) 0

                        keepingOneBlack =
                            Gems 0 0 0 0 1

                        discardingOneWhite =
                            Gems 0 0 0 1 0
                    in
                    BuyTheColors.discardGem White discardGemsState
                        |> Expect.equal (DiscardGemsState keepingOneBlack discardingOneWhite 0)
            ]
        , describe "paying for a card"
            [ test "paying for a card with a gem will add 1 to table gems" <|
                \_ ->
                    -- When paying with 1 Red gem, the player should have 1 less red gem
                    -- and the gamestate should have 1 more
                    let
                        result =
                            BuyTheColors.payWithAGem redGemPayment purchaseStateRedGems
                    in
                    result.tableGems.red
                        |> Expect.equal 2
            , test "paying for a card with a gem will subtract 1 from player gems" <|
                \_ ->
                    let
                        result =
                            BuyTheColors.payWithAGem redGemPayment purchaseStateRedGems
                    in
                    result.playerGems.red
                        |> Expect.equal 2
            , test "paying for a card with a gem will subtract 1 from card cost" <|
                \_ ->
                    let
                        result =
                            BuyTheColors.payWithAGem redGemPayment purchaseStateRedGems
                    in
                    result.cardToPurchase.card.price.red
                        |> Expect.equal 2
            ]
        , describe "completing a card purchase"
            [ test "when a player is purchasing a reserved card, the card will be removed from their reserved cards" <|
                \_ ->
                    let
                        result =
                            BuyTheColors.completeCardPurchase purchaseStateWithCardPaid playerPurchasing
                                |> toPlayer
                    in
                    result.reservedCards
                        |> Expect.equalLists []
            , test "when a player is purchasing a reserved card, the card will be added to their purchased cards" <|
                \_ ->
                    let
                        result =
                            BuyTheColors.completeCardPurchase purchaseStateWithCardPaid playerPurchasing
                                |> toPlayer
                    in
                    result.cards
                        |> Expect.equalLists [ cardWithCostOf3RedGems ]
            , test "when a player is purchasing a reserved card, the player's gems are set to the purchaseState value" <|
                \_ ->
                    let
                        result =
                            BuyTheColors.completeCardPurchase purchaseStateWithCardPaid playerPurchasing
                                |> toPlayer

                        purchaseStateGems =
                            purchaseStateWithCardPaid
                    in
                    result.gems
                        |> Expect.equal purchaseStateGems.playerGems
            , test "when a player is purchasing a reserved card, the player's wilds are set to the purchaseState value" <|
                \_ ->
                    let
                        playerWithWilds =
                            playerPurchasing
                                |> withWilds

                        purchaseState =
                            purchaseStateWithWilds

                        result =
                            BuyTheColors.completeCardPurchase purchaseState playerWithWilds
                                |> toPlayer
                    in
                    result.wilds
                        |> Expect.equal purchaseState.playerWilds
            , test "when a player is purchasing a reserved card, the tableGems are set to the purchaseState value" <|
                \_ ->
                    let
                        result =
                            BuyTheColors.completeCardPurchase purchaseStateWithCardPaid playerPurchasing
                                |> toTableGems

                        purchaseStateGems =
                            purchaseStateWithCardPaid
                    in
                    result
                        |> Expect.equal purchaseStateGems.tableGems
            ]
        , describe "reserving a card"
            [ test "when a player pays for a card, and then immediately reserves it, the card price should revert back to its initial price" <|
                \_ ->
                    let
                        purchaseState =
                            { purchaseStateRedGems
                                | cardToPurchase =
                                    Cards.BuyableCard cardWithGemsPaid Nothing True
                            }

                        expectedPrice =
                            cardWithCostOf3RedGems.price

                        result =
                            BuyTheColors.reserveCardFromPurchaseState purchaseState playerPurchasing
                                |> firstReservedCardPrice
                    in
                    case result of
                        Just actualPrice ->
                            Expect.equal actualPrice expectedPrice

                        _ ->
                            Expect.fail "Player has no reserved cards. This is a problem with the unit test itself."
            ]
        ]


firstReservedCardPrice : Player -> Maybe Gems
firstReservedCardPrice player =
    case List.head player.reservedCards of
        Just card ->
            Just card.price

        _ ->
            Nothing


playerPurchasing : Player
playerPurchasing =
    { name = "player1"
    , gems =
        Gems.initEmptyGems
            |> Gems.setGemAmount Red 3
    , wilds = 0
    , cards = []
    , reservedCards =
        [ cardWithCostOf3RedGems ]
    , noblesVisited = []
    }


withWilds : Player -> Player
withWilds player =
    { player | wilds = 1 }


purchaseStateRedGems : BuyTheColors.PurchaseState
purchaseStateRedGems =
    let
        player =
            playerPurchasing

        price =
            Gems.initEmptyGems
                |> Gems.setGemAmount Red 3
    in
    { playerGems = player.gems
    , playerWilds = 0
    , tableGems =
        Gems.initEmptyGems
            |> Gems.setGemAmount Red 1
    , cardToPurchase =
        Cards.BuyableCard cardWithCostOf3RedGems Nothing True
    , originalCardPrice = price
    }


purchaseStateWithCardPaid : BuyTheColors.PurchaseState
purchaseStateWithCardPaid =
    let
        cardPaid =
            { cardWithCostOf3RedGems
                | price = Gems 0 0 0 0 0
            }
    in
    { purchaseStateRedGems
        | cardToPurchase = Cards.BuyableCard cardPaid Nothing True
    }


purchaseStateWithWilds : BuyTheColors.PurchaseState
purchaseStateWithWilds =
    let
        player =
            playerPurchasing

        price =
            Gems.initEmptyGems
                |> Gems.setGemAmount Red 3
    in
    { playerGems = player.gems
    , playerWilds = 0
    , tableGems =
        Gems.initEmptyGems
            |> Gems.setGemAmount Red 1
    , cardToPurchase =
        Cards.BuyableCard cardWithCostOf3RedGems Nothing True
    , originalCardPrice = price
    }


toPlayer : ( Player, TableGems ) -> Player
toPlayer ( player, tableGems ) =
    player


toTableGems : ( Player, TableGems ) -> TableGems
toTableGems ( player, tableGems ) =
    tableGems


redGemPayment : BuyTheColors.Payment
redGemPayment =
    BuyTheColors.Payment BuyTheColors.GemType Red


cardWithCostOf3RedGems : Card
cardWithCostOf3RedGems =
    Card (Gems 3 0 0 0 0) Blue 1 "" Level1


cardWithGemsPaid : Card
cardWithGemsPaid =
    Card (Gems 0 0 0 0 0) Blue 1 "" Level1
