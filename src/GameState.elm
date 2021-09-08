module GameState exposing (GameState, encodeGameState, gameStateDecoder, updatePlayerInGameState)

import Cards exposing (..)
import Nobles exposing (..)
import Gems exposing (..)
import Json.Decode as D exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Encode as E exposing (..)
import Player exposing (..)


type alias GameState =
    { tableGems : Gems
    , decks : Decks
    , nobles : List Noble
    , lvl1CardsDrawn : RowOfBuyableCards
    , lvl2CardsDrawn : RowOfBuyableCards
    , lvl3CardsDrawn : RowOfBuyableCards
    , numberOfTurns : Int
    , gameLog : List String
    , allPlayers : List Player
    }


updatePlayerInGameState : Player -> GameState -> GameState
updatePlayerInGameState player gameState =
    { gameState | allPlayers = updatePlayer player gameState.allPlayers }


encodeGameState : GameState -> E.Value
encodeGameState gameState =
    E.object
        [ ( "tableGems", encodeGems gameState.tableGems )
        , ( "decks", encodeDecks gameState.decks )
        , ( "nobles", E.list encodeNoble gameState.nobles )
        , ( "lvl1CardsDrawn", encodeRowOfBuyableCards gameState.lvl1CardsDrawn )
        , ( "lvl2CardsDrawn", encodeRowOfBuyableCards gameState.lvl2CardsDrawn )
        , ( "lvl3CardsDrawn", encodeRowOfBuyableCards gameState.lvl3CardsDrawn )
        , ( "numberOfTurns", E.int gameState.numberOfTurns )
        , ( "gameLog", E.list E.string gameState.gameLog )
        , ( "allPlayers", E.list encodePlayer gameState.allPlayers )
        ]


gameStateDecoder : D.Decoder GameState
gameStateDecoder =
    succeed GameState
        |> andMap (field "tableGems" gemsDecoder)
        |> andMap (field "decks" decksDecoder)
        |> andMap (field "nobles" (D.list nobleDecoder))
        |> andMap (field "lvl1CardsDrawn" rowOfBuyableCardsDecoder)
        |> andMap (field "lvl2CardsDrawn" rowOfBuyableCardsDecoder)
        |> andMap (field "lvl3CardsDrawn" rowOfBuyableCardsDecoder)
        |> andMap (field "numberOfTurns" D.int)
        |> andMap (field "gameLog" (D.list D.string))
        |> andMap (field "allPlayers" (D.list playerDecoder))
