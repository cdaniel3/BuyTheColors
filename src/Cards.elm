module Cards exposing (..)

import Array
import Gems exposing (..)
import Json.Decode as D exposing (..)
import Json.Decode.Extra as DE exposing (..)
import Json.Encode as E exposing (..)
import Json.Encode.Extra as EE exposing (..)
import Random
import Random.Extra as RandomExtra exposing (..)
import Random.List exposing (..)


type Msg
    = DrawCard
    | DealCards Decks



-- Type aliases


type alias Card =
    { price : Gems
    , discount : GemColor
    , victoryPoints : Int
    , url : String
    , level : DeckLevel
    }


type alias Decks =
    { lvl1Deck : List Card
    , lvl2Deck : List Card
    , lvl3Deck : List Card
    }


type alias Noble =
    { visitAmount : Gems
    , victoryPoints : Int
    , url : String
    }


type DeckLevel
    = Level1
    | Level2
    | Level3


type BuyableCardLocation
    = LeftCard
    | MiddleLeftCard
    | MiddleRightCard
    | RightCard


type alias BuyableCard =
    { card : Card
    , location : Maybe BuyableCardLocation
    , isReservable : Bool
    }


type alias RowOfBuyableCards =
    { left : Maybe Card
    , middleLeft : Maybe Card
    , middleRight : Maybe Card
    , right : Maybe Card
    }


type alias Model =
    { decks : Decks
    , lvl1CardsDrawn : RowOfBuyableCards
    , lvl2CardsDrawn : RowOfBuyableCards
    , lvl3CardsDrawn : RowOfBuyableCards
    , drawnCard : Maybe Card
    }


deal : List Card -> ( RowOfBuyableCards, List Card )
deal cards =
    let
        ( drawn, remaining ) =
            draw 4 cards

        cardsDrawn =
            Array.fromList drawn
    in
    ( { left = Array.get 0 cardsDrawn
      , middleLeft = Array.get 1 cardsDrawn
      , middleRight = Array.get 2 cardsDrawn
      , right = Array.get 3 cardsDrawn
      }
    , remaining
    )


draw : Int -> List Card -> ( List Card, List Card )
draw amt cards =
    ( List.take amt cards, List.drop amt cards )


drawOne : List Card -> ( Maybe Card, List Card )
drawOne cards =
    let
        ( cardsDrawn, remaining ) =
            draw 1 cards
    in
    let
        drawn =
            List.head cardsDrawn
    in
    ( drawn, remaining )


shuffleDecks : Decks -> Random.Generator Decks
shuffleDecks decks =
    Random.map Decks (shuffle decks.lvl1Deck)
        |> RandomExtra.andMap (shuffle decks.lvl2Deck)
        |> RandomExtra.andMap (shuffle decks.lvl3Deck)


replaceCardInBuyableCards : BuyableCardLocation -> Maybe Card -> RowOfBuyableCards -> RowOfBuyableCards
replaceCardInBuyableCards location maybeCard row =
    case location of
        LeftCard ->
            { row | left = maybeCard }

        MiddleLeftCard ->
            { row | middleLeft = maybeCard }

        MiddleRightCard ->
            { row | middleRight = maybeCard }

        RightCard ->
            { row | right = maybeCard }


cardEquals : Card -> Card -> Bool
cardEquals cardA cardB =
    let
        priceEquals =
            Gems.gemsEquals cardA.price cardB.price

        discountEquals =
            cardA.discount == cardB.discount

        victoryPointsEquals =
            cardA.victoryPoints == cardB.victoryPoints

        levelEquals =
            cardA.level == cardB.level
    in
    priceEquals
        && discountEquals
        && victoryPointsEquals
        && levelEquals


encodeCard : Card -> E.Value
encodeCard card =
    E.object
        [ ( "price", encodeGems card.price )
        , ( "discount", E.string (Gems.toString card.discount) )
        , ( "victoryPoints", E.int card.victoryPoints )
        , ( "url", E.string card.url )
        , ( "level"
          , E.string <|
                case card.level of
                    Level1 ->
                        "Level1"

                    Level2 ->
                        "Level2"

                    Level3 ->
                        "Level3"
          )
        ]


cardDecoder : D.Decoder Card
cardDecoder =
    succeed Card
        |> DE.andMap (field "price" gemsDecoder)
        |> DE.andMap (field "discount" gemColorDecoder)
        |> DE.andMap (field "victoryPoints" D.int)
        |> DE.andMap (field "url" D.string)
        |> DE.andMap (field "level" deckLevelDecoder)


deckLevelDecoder : D.Decoder DeckLevel
deckLevelDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "Level1" ->
                        succeed Level1

                    "Level2" ->
                        succeed Level2

                    "Level3" ->
                        succeed Level3

                    somethingElse ->
                        D.fail ("unknown deck level" ++ somethingElse)
            )


encodeDecks : Decks -> E.Value
encodeDecks decks =
    E.object
        [ ( "lvl1Deck", E.list encodeCard decks.lvl1Deck )
        , ( "lvl2Deck", E.list encodeCard decks.lvl2Deck )
        , ( "lvl3Deck", E.list encodeCard decks.lvl3Deck )
        ]


decksDecoder : Decoder Decks
decksDecoder =
    succeed Decks
        |> DE.andMap (field "lvl1Deck" (D.list cardDecoder))
        |> DE.andMap (field "lvl2Deck" (D.list cardDecoder))
        |> DE.andMap (field "lvl3Deck" (D.list cardDecoder))


encodeRowOfBuyableCards : RowOfBuyableCards -> E.Value
encodeRowOfBuyableCards row =
    E.object
        [ ( "left", EE.maybe (\card -> encodeCard card) row.left )
        , ( "middleLeft", EE.maybe (\card -> encodeCard card) row.middleLeft )
        , ( "middleRight", EE.maybe (\card -> encodeCard card) row.middleRight )
        , ( "right", EE.maybe (\card -> encodeCard card) row.right )
        ]


rowOfBuyableCardsDecoder : D.Decoder RowOfBuyableCards
rowOfBuyableCardsDecoder =
    succeed RowOfBuyableCards
        |> DE.andMap (optionalNullableField "left" cardDecoder)
        |> DE.andMap (optionalNullableField "middleLeft" cardDecoder)
        |> DE.andMap (optionalNullableField "middleRight" cardDecoder)
        |> DE.andMap (optionalNullableField "right" cardDecoder)


noCardsDrawn : RowOfBuyableCards
noCardsDrawn =
    RowOfBuyableCards Nothing Nothing Nothing Nothing


initFrom : String -> String -> Card
initFrom deckName cardName =
    let
        card =
            String.split "_" cardName
                |> List.indexedMap Tuple.pair
                |> List.foldl byCardParameters initCard
    in
    { card
        | url = deckName ++ "/" ++ cardName ++ ".png"
    }


byCardParameters : ( Int, String ) -> Card -> Card
byCardParameters ( idx, param ) card =
    case idx of
        0 ->
            -- Gems: first 5 characters
            let
                price =
                    String.toList param
                        |> List.map (\c -> String.fromChar c)
                        |> List.map (\s -> Maybe.withDefault 0 (String.toInt s))
                        |> List.indexedMap Tuple.pair
                        |> List.foldl byGemParameters initEmptyGems
            in
            { card | price = price }

        1 ->
            -- VP
            { card
                | victoryPoints =
                    Maybe.withDefault 0 (String.toInt param)
            }

        2 ->
            -- Discount (GemColor)
            case Gems.fromString param of
                Just gemColor ->
                    { card | discount = gemColor }

                Nothing ->
                    card

        3 ->
            -- Level
            { card
                | level =
                    case param of
                        "1" ->
                            Level1

                        "2" ->
                            Level2

                        "3" ->
                            Level3

                        _ ->
                            Level1
            }

        _ ->
            card


byGemParameters : ( Int, Int ) -> Gems -> Gems
byGemParameters ( idx, gemCount ) gems =
    case idx of
        0 ->
            setGemAmount Red gemCount gems

        1 ->
            setGemAmount Blue gemCount gems

        2 ->
            setGemAmount Green gemCount gems

        3 ->
            setGemAmount White gemCount gems

        4 ->
            setGemAmount Black gemCount gems

        _ ->
            gems


initCard : Card
initCard =
    { price = initEmptyGems
    , discount = Blue
    , victoryPoints = 0
    , url = ""
    , level = Level1
    }


sortByDiscount : List Card -> List Card
sortByDiscount cards =
    List.sortBy
        (\card ->
            case card.discount of
                White ->
                    0

                Blue ->
                    1

                Green ->
                    2

                Red ->
                    3

                Black ->
                    4
        )
        cards
