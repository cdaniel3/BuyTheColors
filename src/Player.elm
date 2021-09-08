module Player exposing (..)

import Cards exposing (..)
import Gems exposing (..)
import Nobles exposing (..)
import Json.Decode as D exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Encode as E exposing (..)


type alias Player =
    { name : String
    , gems : Gems
    , wilds : Int
    , cards : List Card
    , reservedCards : List Card
    , noblesVisited : List Noble
    }


initPlayer : Player
initPlayer =
    { name = ""
    , gems = initEmptyGems
    , wilds = 0
    , cards = []
    , reservedCards = []
    , noblesVisited = []
    }



-- initPlayers : List Player
-- initPlayers =
--     [ { initPlayer | name = "Cory" }
--     , { initPlayer
--         | name = "Cody"
--         , gems = { initEmptyGems | red = 2 }
--         , cards = [ initFrom "Level1Deck" "00040_1_Red_1" ]
--         , reservedCards =
--             [ initFrom "Level1Deck" "00220_0_Black_1"
--             , initFrom "Level1Deck" "11012_0_Green_1"
--             ]
--       }
--     , { initPlayer
--         | name = "Bryan"
--         , gems = { initEmptyGems | blue = 3 }
--         , cards = [ initFrom "Level3Deck" "03070_5_Blue_3" ]
--         , reservedCards =
--             [ initFrom "Level1Deck" "11012_0_Green_1"
--             , initFrom "Level3Deck" "03070_5_Blue_3"
--             , initFrom "Level2Deck" "00050_2_Black_2"
--             ]
--       }
--     ]


initPlayersFromNames : List String -> List Player
initPlayersFromNames names =
    List.map (\name -> { initPlayer | name = name }) names


findPlayerByName : String -> List Player -> Maybe Player
findPlayerByName name players =
    List.filter (\player -> player.name == name) players
        |> List.head


updatePlayer : Player -> List Player -> List Player
updatePlayer updated players =
    let
        otherPlayers =
            List.filter (\player -> player.name /= updated.name) players
    in
    updated :: otherPlayers


addCardToPlayer : Card -> Player -> Player
addCardToPlayer card player =
    let
        otherReservedCards =
            not << Cards.cardEquals card
    in
    { player
        | cards = card :: player.cards
        , reservedCards = List.filter otherReservedCards player.reservedCards
    }


addCardToPlayerReserve : Card -> Player -> Player
addCardToPlayerReserve card player =
    { player
        | reservedCards = card :: player.reservedCards
    }


addWildToPlayer : Player -> Player
addWildToPlayer player =
    let
        wilds =
            player.wilds
    in
    { player | wilds = wilds + 1 }


addNoblesToPlayer : List Noble -> Player -> Player
addNoblesToPlayer nobles player =
    let
        noblesVisited =
            player.noblesVisited
    in
    { player | noblesVisited = noblesVisited ++ nobles }


victoryPoints : Player -> Int
victoryPoints player =
    List.map (\card -> card.victoryPoints) player.cards
        |> List.append (List.map (\noble -> noble.victoryPoints) player.noblesVisited)
        |> List.sum


discounts : Player -> List GemColor
discounts player =
    List.filterMap (\card -> Just card.discount) player.cards


nobleVisit : List Noble -> Player -> ( List Noble, List Noble )
nobleVisit nobles player =
    let
        playerDiscounts =
            discounts player
    in
    List.partition
        (\noble ->
            isVisiting playerDiscounts noble.visitAmount
        )
        nobles


isVisiting : List GemColor -> Gems -> Bool
isVisiting owned visitAmount =
    let
        ( _, updatedVisitAmount ) =
            checkForVisit owned visitAmount
    in
    totalGemCount updatedVisitAmount <= 0


checkForVisit : List GemColor -> Gems -> ( List GemColor, Gems )
checkForVisit gemsToCheck requiredGems =
    -- base case: gemsToCheck.length is 0 -OR- requiredGems is empty
    -- if requiredGems is empty, visit occurs
    case List.head gemsToCheck of
        Just gemToCheck ->
            let
                visitAmountForThisGem =
                    getGemAmount gemToCheck requiredGems

                updatedRequiredGems =
                    if visitAmountForThisGem > 0 then
                        minusOneGem gemToCheck requiredGems

                    else
                        requiredGems
            in
            checkForVisit (List.drop 1 gemsToCheck) updatedRequiredGems

        Nothing ->
            ( [], requiredGems )


isActivePlayer : Player -> List Player -> Bool
isActivePlayer player players =
    case getActivePlayer players of
        Just activePlayer ->
            activePlayer.name == player.name

        Nothing ->
            False


getActivePlayer : List Player -> Maybe Player
getActivePlayer players =
    List.head players


getPlayerNameWithMostPoints : List Player -> Maybe String
getPlayerNameWithMostPoints players =
    let
        playerWithMost =
            List.sortBy (\player -> victoryPoints player) players
                |> List.reverse
                |> List.head
    in
    Maybe.map (\player -> player.name) playerWithMost


encodePlayer : Player -> E.Value
encodePlayer player =
    E.object
        [ ( "name", E.string player.name )
        , ( "gems", encodeGems player.gems )
        , ( "wilds", E.int player.wilds )
        , ( "cards", E.list encodeCard player.cards )
        , ( "reservedCards", E.list encodeCard player.reservedCards )
        , ( "noblesVisited", E.list encodeNoble player.noblesVisited )
        ]


playerDecoder : D.Decoder Player
playerDecoder =
    succeed Player
        |> andMap (field "name" D.string)
        |> andMap (field "gems" gemsDecoder)
        |> andMap (field "wilds" D.int)
        |> andMap (field "cards" (D.list cardDecoder))
        |> andMap (field "reservedCards" (D.list cardDecoder))
        |> andMap (field "noblesVisited" (D.list nobleDecoder))
