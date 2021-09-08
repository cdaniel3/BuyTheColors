port module BuyTheColors exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Cards exposing (BuyableCard, Decks, Noble, noCardsDrawn, shuffleDecks, deal, DeckLevel(..), drawOne, replaceCardInBuyableCards, Card, sortByDiscount, RowOfBuyableCards, BuyableCardLocation(..))
import Decks.Level1Deck exposing (level1Deck)
import Decks.Level2Deck exposing (level2Deck)
import Decks.Level3Deck exposing (level3Deck)
import GameState exposing (GameState, encodeGameState, gameStateDecoder, updatePlayerInGameState)
import Gems exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, disabled, placeholder, src, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as D
import Json.Decode.Extra as DE exposing (..)
import Json.Encode as E
import Nobles exposing (..)
import Player exposing (..)
import Random
import Random.List exposing (shuffle)
import Html.Attributes exposing (width)


isSingleBrowserGame : Bool
isSingleBrowserGame =
    True


victoryPointsToWin : Int
victoryPointsToWin =
    15


playerMaxGems : Int
playerMaxGems =
    10


type Msg
    = PlayerNameChanged String
    | RegisterPlayer
    | StartGame
    | GrabGem GemColor
    | ConfirmGrabGem (List GemColor)
    | CancelGrabGem
    | DiscardedGem DiscardedGemType
    | ConfirmDiscardedGems
    | PayForCard Payment
    | ConfirmCardPurchase BuyableCard
    | ResetPurchase
    | ResetDiscardedGems
    | ViewCardModal BuyableCard
    | ReserveCard BuyableCard
    | DealCards Decks
    | DealNobles (List Noble)
    | IncomingState D.Value
    | HideModal
    | NotifyOnTurn Bool
    | ShowCompletedGameModal


type alias Payment =
    { paymentType : PaymentType
    , gemColor : GemColor
    }


type PaymentType
    = GemType
    | WildType


type DiscardedGemType
    = DiscardedGemType GemColor
    | DiscardedWildType


type IncomingType
    = GotGameState GameState
    | GotNotification Notification
    | GotStartGame (List String)
    | GotNewlyRegisteredPlayer String
    | GotRegisteredPlayers (List String)


type GemActionType
    = TwoOfSameColor
    | ThreeOfDifferentColor
    | Available



-- Maybe look at unpacking structure in FollowedAuthor & UnfollowedAuthor (elm workshop part5)
-- like CardAvailable or BuyableCard vs PurchasedCard


type alias PurchaseState =
    { playerGems : Gems
    , tableGems : Gems
    , playerWilds : Int
    , cardToPurchase : BuyableCard
    , originalCardPrice : Gems
    }


type alias DiscardGemsState =
    { gemsToKeep : Gems
    , gemsToDiscard : Gems
    , wildsToKeep : Int
    }


type alias Notification =
    { title : String
    , message : String
    }



-- MODEL
-- Maybe use a custom type for "registering" or "playing" or "completed" - (elm workshop part5)


type Model
    = Registering RegisterModel
    | Playing CurrentGameModel
    | Completed CompletedGameModel


type alias RegisterModel =
    { myName : String
    , errorMessage : String
    , notificationMessage : String
    , playerNames : List String
    }


type alias CurrentGameModel =
    { gameState : GameState

    -- The user playing the game on their browser / client
    , clientPlayerName : String
    , playerActionType : Maybe PlayerActionType
    , notifyOnPlayerTurn : Bool
    , notificationMsg : String
    , buyTheColorsModal : BuyTheColorsModal
    }


type alias CompletedGameModel =
    { gameState : GameState
    , clientPlayerName : String
    , buyTheColorsModal : BuyTheColorsModal
    , notificationMsg : String
    }


type PlayerActionType
    = PurchaseType PurchaseState
    | GrabGemsType (List GemColor)
    | DiscardGemsType DiscardGemsState


type alias BuyTheColorsModal =
    { header : String
    , content : List (Html Msg)
    , footer : List (Html Msg)
    , modalVisibility : Modal.Visibility
    , close : Msg
    }


type alias TableGems =
    Gems


type alias PlayerGems =
    Gems



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port sendMessage : String -> Cmd msg


port messageReceiver : (D.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver IncomingState


initRegisteringModel : () -> ( Model, Cmd Msg )
initRegisteringModel _ =
    ( Registering
        { myName = ""
        , errorMessage = ""
        , notificationMessage = ""
        , playerNames = []
        }
    , Cmd.none
    )


init : () -> ( Model, Cmd Msg )
init _ =
    -- Websockets: initRegisteringModel ()
    -- Browser testing: initCurrentGameModel
    if isSingleBrowserGame then
        initCurrentGameModel

    else
        initRegisteringModel ()


fromCurrentGameModel : ( CurrentGameModel, Cmd Msg ) -> ( Model, Cmd Msg )
fromCurrentGameModel ( currentGame, cmd ) =
    if isGameComplete currentGame.gameState then
        let
            completedGame =
                { gameState = currentGame.gameState
                , clientPlayerName = currentGame.clientPlayerName
                , notificationMsg = currentGame.notificationMsg
                , buyTheColorsModal = completedGameContent currentGame.gameState.allPlayers
                }
        in
        ( Completed completedGame, cmd )

    else
        ( Playing currentGame, cmd )


fromCompletedGameModel : ( CompletedGameModel, Cmd Msg ) -> ( Model, Cmd Msg )
fromCompletedGameModel ( completedGameModel, cmd ) =
    ( Completed completedGameModel, cmd )


isGameComplete : GameState -> Bool
isGameComplete gameState =
    List.any (\player -> Player.victoryPoints player >= victoryPointsToWin) gameState.allPlayers


fromRegisterModel : ( RegisterModel, Cmd Msg ) -> ( Model, Cmd Msg )
fromRegisterModel ( registerModel, cmd ) =
    ( Registering registerModel, cmd )


initFromGameStateAndPlayerName : GameState -> String -> Model
initFromGameStateAndPlayerName gameState playerName =
    Playing
        { gameState = gameState
        , clientPlayerName = playerName
        , playerActionType = Nothing
        , notificationMsg = ""
        , notifyOnPlayerTurn = False
        , buyTheColorsModal =
            { header = ""
            , content = [ text "" ]
            , footer = [ text "" ]
            , modalVisibility = Modal.hidden
            , close = ResetPurchase
            }
        }


initGameState : List String -> GameState
initGameState playerNames =
    let
        playerCount =
            List.length playerNames

        eachTableGemAmount =
            -- 7 for 4p, 5 for 3p, 4 for 2p
            if playerCount == 4 then
                7

            else if playerCount == 3 then
                5

            else
                4
    in
    { tableGems =
        { red = eachTableGemAmount
        , blue = eachTableGemAmount
        , green = eachTableGemAmount
        , white = eachTableGemAmount
        , black = eachTableGemAmount
        }
    , decks = Decks level1Deck level2Deck level3Deck
    , nobles = [ noble4B4G3Points ]
    , lvl1CardsDrawn = noCardsDrawn
    , lvl2CardsDrawn = noCardsDrawn
    , lvl3CardsDrawn = noCardsDrawn
    , numberOfTurns = 0
    , gameLog = []
    , allPlayers = initPlayersFromNames playerNames
    }



-- Check for errors with lvl 1/2/3 decks and display some sort of error state
-- instead of Playing model


initCurrentGameModel : ( Model, Cmd Msg )
initCurrentGameModel =
    let
        initDecks =
            Decks level1Deck level2Deck level3Deck
    in
    ( Playing
        { gameState = initGameState [ "player1", "player2" ]
        , clientPlayerName = "player1"
        , playerActionType = Nothing
        , notificationMsg = ""
        , notifyOnPlayerTurn = False
        , buyTheColorsModal =
            { header = ""
            , content = [ text "" ]
            , footer = [ text "" ]
            , modalVisibility = Modal.hidden
            , close = ResetPurchase
            }
        }
    , Cmd.batch
        [ initDecks
            |> shuffleDecks
            |> Random.generate DealCards
        , shuffle Nobles.allNobles
            |> Random.generate DealNobles
        ]
    )


initFromPlayerName : String -> List String -> Model
initFromPlayerName playerName otherPlayerNames =
    Playing
        { gameState = initGameState (playerName :: otherPlayerNames)
        , clientPlayerName = playerName
        , playerActionType = Nothing
        , notificationMsg = ""
        , notifyOnPlayerTurn = False
        , buyTheColorsModal =
            { header = ""
            , content = [ text "" ]
            , footer = [ text "" ]
            , modalVisibility = Modal.hidden
            , close = ResetPurchase
            }
        }


startGame : String -> List String -> ( Model, Cmd Msg )
startGame firstPlayerName otherPlayerNames =
    let
        initDecks =
            Decks level1Deck level2Deck level3Deck
    in
    ( initFromPlayerName firstPlayerName otherPlayerNames
    , Cmd.batch
        [ initDecks
            |> shuffleDecks
            |> Random.generate DealCards
        , shuffle Nobles.allNobles
            |> Random.generate DealNobles
        ]
    )


clearNotificationMsg : CurrentGameModel -> CurrentGameModel
clearNotificationMsg model =
    { model | notificationMsg = "" }



-- This updates the client player (browser client) to be the active player


updateClientPlayer : CurrentGameModel -> CurrentGameModel
updateClientPlayer currentGame =
    if isSingleBrowserGame then
        case List.head currentGame.gameState.allPlayers of
            Just activePlayer ->
                { currentGame | clientPlayerName = activePlayer.name }

            Nothing ->
                currentGame

    else
        currentGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Registering registerModel ->
            updateRegister msg registerModel

        Playing currentGame ->
            updateCurrentGame msg currentGame
                |> fromCurrentGameModel

        Completed completedGame ->
            updateCompletedGame msg completedGame
                |> fromCompletedGameModel


updateRegister : Msg -> RegisterModel -> ( Model, Cmd Msg )
updateRegister msg registerModel =
    case msg of
        PlayerNameChanged draft ->
            ( { registerModel | myName = draft }
            , Cmd.none
            )
                |> fromRegisterModel

        RegisterPlayer ->
            ( { registerModel
                | notificationMessage = "Waiting for other player..."
              }
            , sendMessage (wrapEncoded (E.string registerModel.myName) "register")
            )
                |> fromRegisterModel

        StartGame ->
            ( registerModel
            , sendMessage (wrapEncoded (E.list E.string registerModel.playerNames) "start")
            )
                |> fromRegisterModel

        IncomingState received ->
            case D.decodeValue incomingDecoder received of
                Ok result ->
                    case result of
                        GotGameState gameState ->
                            ( initFromGameStateAndPlayerName gameState registerModel.myName
                            , Cmd.none
                            )

                        GotNotification notification ->
                            ( { registerModel | notificationMessage = notification.message }
                            , Cmd.none
                            )
                                |> fromRegisterModel

                        GotStartGame playerNames ->
                            case playerNames of
                                firstPlayerName :: otherPlayerNames ->
                                    if firstPlayerName == registerModel.myName then
                                        startGame firstPlayerName otherPlayerNames

                                    else
                                        ( registerModel, Cmd.none )
                                            |> fromRegisterModel

                                _ ->
                                    -- This would mean an error occurred
                                    ( registerModel, Cmd.none )
                                        |> fromRegisterModel

                        GotNewlyRegisteredPlayer playerName ->
                            let
                                updatedRegisterModel =
                                    { registerModel
                                        | playerNames =
                                            List.append [ playerName ] registerModel.playerNames
                                    }
                            in
                            ( updatedRegisterModel, Cmd.none )
                                |> fromRegisterModel

                        GotRegisteredPlayers players ->
                            let
                                updatedRegisterModel =
                                    { registerModel | playerNames = players }
                            in
                            ( updatedRegisterModel, Cmd.none )
                                |> fromRegisterModel

                Err errorMsg ->
                    ( { registerModel | errorMessage = D.errorToString errorMsg }
                    , Cmd.none
                    )
                        |> fromRegisterModel

        _ ->
            ( { registerModel | errorMessage = "Unexpected msg condition found in RegisterModel" }, Cmd.none )
                |> fromRegisterModel



-- Maybe put into a PlayerTurns module?


confirmGrabGems : TableGems -> PlayerGems -> List GemColor -> ( TableGems, PlayerGems )
confirmGrabGems tableGems playerGems grabbed =
    let
        updatedTableGems =
            List.foldl (\gemColor gems -> minusOneGem gemColor gems) tableGems grabbed

        updatedPlayerGems =
            List.foldl (\gemColor gems -> plusOneGem gemColor gems) playerGems grabbed
    in
    ( updatedTableGems, updatedPlayerGems )


grabGemAndCheckForConfirm : GemColor -> CurrentGameModel -> ( CurrentGameModel, Cmd Msg )
grabGemAndCheckForConfirm gemColor model =
    ( grabGem gemColor model
        |> checkForConfirm
    , Cmd.none
    )


updateCurrentGame : Msg -> CurrentGameModel -> ( CurrentGameModel, Cmd Msg )
updateCurrentGame msg model =
    case findClientPlayer model of
        Just clientPlayer ->
            let
                gameState =
                    model.gameState

                tableGems =
                    gameState.tableGems
            in
            case ( msg, model.playerActionType ) of
                ( GrabGem gemColor, Just (GrabGemsType _) ) ->
                    grabGemAndCheckForConfirm gemColor model

                ( GrabGem gemColor, Nothing ) ->
                    grabGemAndCheckForConfirm gemColor model

                ( ConfirmGrabGem grabbed, Just (GrabGemsType _) ) ->
                    let
                        ( updatedTableGems, updatedPlayerGems ) =
                            confirmGrabGems tableGems clientPlayer.gems grabbed

                        updatedPlayer =
                            { clientPlayer | gems = updatedPlayerGems }
                    in
                    -- After updating the tableGems and allPlayers:
                    --   If the player gems and wilds exceed the max player gems, show the discard gems modal
                    if gemsAndWildCountExceeded updatedPlayer.gems updatedPlayer.wilds then
                        let
                            discardGemsState =
                                DiscardGemsState updatedPlayer.gems initEmptyGems updatedPlayer.wilds
                        in
                        ( { model
                            | gameState =
                                { gameState | tableGems = updatedTableGems }
                                    |> updatePlayerInGameState updatedPlayer
                            , playerActionType = Just (DiscardGemsType discardGemsState)
                            , buyTheColorsModal = discardGemsContent discardGemsState
                          }
                            |> clearNotificationMsg
                        , Cmd.none
                        )

                    else
                        let
                            updatedGameState =
                                { gameState
                                    | tableGems = updatedTableGems
                                }
                                    |> updatePlayerInGameState updatedPlayer
                                    |> logAction (describeGrabGemAction clientPlayer.name grabbed)
                                    |> incrementTurn
                        in
                        ( { model
                            | gameState = updatedGameState
                            , playerActionType = Nothing
                            , buyTheColorsModal = hideModal model.buyTheColorsModal
                          }
                            |> clearNotificationMsg
                            |> updateClientPlayer
                        , sendMessage (wrapEncoded (encodeGameState updatedGameState) "ingame")
                        )

                ( CancelGrabGem, Just (GrabGemsType _) ) ->
                    ( { model
                        | playerActionType = Nothing
                        , notificationMsg = ""
                        , buyTheColorsModal = hideModal model.buyTheColorsModal
                      }
                    , Cmd.none
                    )

                ( PayForCard payment, Just (PurchaseType purchaseState) ) ->
                    let
                        newPurchaseState =
                            payWithAGem payment purchaseState

                        player =
                            { clientPlayer
                                | gems = newPurchaseState.playerGems
                                , wilds = newPurchaseState.playerWilds
                            }
                    in
                    ( { model
                        | playerActionType = Just (PurchaseType newPurchaseState)
                        , buyTheColorsModal = toCardPurchaseContent player newPurchaseState
                      }
                    , Cmd.none
                    )

                ( ConfirmCardPurchase buyableCard, Just (PurchaseType purchaseState) ) ->
                    let
                        ( playerWithCard, updatedTableGems ) =
                            completeCardPurchase purchaseState clientPlayer

                        ( noblesVisited, noblesRemaining ) =
                            nobleVisit gameState.nobles playerWithCard

                        updatedPlayer =
                            addNoblesToPlayer noblesVisited playerWithCard
                    in
                    let
                        updatedGameState =
                            { gameState
                                | nobles = noblesRemaining
                                , tableGems = updatedTableGems
                            }
                                |> updatePlayerInGameState updatedPlayer
                                |> replaceBuyableCard buyableCard
                                |> logAction (clientPlayer.name ++ " purchased a card")
                                |> incrementTurn
                    in
                    ( { model
                        | gameState = updatedGameState
                        , playerActionType = Nothing
                        , buyTheColorsModal = hideModal model.buyTheColorsModal
                      }
                        |> clearNotificationMsg
                        |> updateClientPlayer
                    , sendMessage (wrapEncoded (encodeGameState updatedGameState) "ingame")
                    )

                ( DiscardedGem discardedGem, Just (DiscardGemsType discardGemsState) ) ->
                    let
                        updatedDiscardGemsState =
                            case discardedGem of
                                DiscardedGemType gemColor ->
                                    discardGem gemColor discardGemsState

                                DiscardedWildType ->
                                    discardWild discardGemsState
                    in
                    ( { model
                        | playerActionType = Just (DiscardGemsType updatedDiscardGemsState)
                        , buyTheColorsModal = discardGemsContent updatedDiscardGemsState
                      }
                    , Cmd.none
                    )

                ( ConfirmDiscardedGems, Just (DiscardGemsType discardGemsState) ) ->
                    let
                        updatedPlayer =
                            { clientPlayer
                                | gems = discardGemsState.gemsToKeep
                                , wilds = discardGemsState.wildsToKeep
                            }

                        updatedGameState =
                            { gameState
                                | tableGems = Gems.addAll tableGems discardGemsState.gemsToDiscard
                            }
                                |> updatePlayerInGameState updatedPlayer
                                |> incrementTurn
                                |> logAction (clientPlayer.name ++ " grabbed and discarded gems")
                    in
                    ( { model
                        | playerActionType = Nothing
                        , buyTheColorsModal = hideModal model.buyTheColorsModal
                        , gameState = updatedGameState
                      }
                        |> updateClientPlayer
                    , sendMessage (wrapEncoded (encodeGameState updatedGameState) "ingame")
                    )

                ( ReserveCard buyableCard, Just (PurchaseType purchaseState) ) ->
                    updateReserveCard model clientPlayer buyableCard purchaseState

                ( ResetPurchase, Just (PurchaseType _) ) ->
                    ( { model
                        | playerActionType = Nothing
                        , buyTheColorsModal = hideModal model.buyTheColorsModal
                      }
                    , Cmd.none
                    )

                ( ResetDiscardedGems, Just (DiscardGemsType _) ) ->
                    -- Reset the discarded ones to what's in the gameState
                    let
                        updatedDiscardGemsState =
                            DiscardGemsState clientPlayer.gems initEmptyGems clientPlayer.wilds
                    in
                    ( { model
                        | playerActionType = Just (DiscardGemsType updatedDiscardGemsState)
                        , buyTheColorsModal = discardGemsContent updatedDiscardGemsState
                      }
                    , Cmd.none
                    )

                ( ViewCardModal buyableCard, _ ) ->
                    ( { model
                        | buyTheColorsModal = cardPurchaseContent buyableCard clientPlayer
                        , playerActionType =
                            Just
                                (PurchaseType
                                    { playerGems = clientPlayer.gems
                                    , tableGems = gameState.tableGems
                                    , playerWilds = clientPlayer.wilds
                                    , cardToPurchase = buyableCard
                                    , originalCardPrice = buyableCard.card.price
                                    }
                                )
                      }
                    , Cmd.none
                    )

                ( DealCards decks, _ ) ->
                    let
                        ( lvl1Drawn, lvl1Remaining ) =
                            deal decks.lvl1Deck

                        ( lvl2Drawn, lvl2Remaining ) =
                            deal decks.lvl2Deck

                        ( lvl3Drawn, lvl3Remaining ) =
                            deal decks.lvl3Deck

                        updatedGameState =
                            { gameState
                                | lvl1CardsDrawn = lvl1Drawn
                                , lvl2CardsDrawn = lvl2Drawn
                                , lvl3CardsDrawn = lvl3Drawn
                                , decks =
                                    { lvl1Deck = lvl1Remaining
                                    , lvl2Deck = lvl2Remaining
                                    , lvl3Deck = lvl3Remaining
                                    }
                            }
                    in
                    ( { model
                        | gameState = updatedGameState
                      }
                    , sendMessage (wrapEncoded (encodeGameState updatedGameState) "ingame")
                    )

                ( DealNobles allNobles, _ ) ->
                    let
                        playerCount =
                            List.length gameState.allPlayers

                        noblesAmount =
                            playerCount + 1

                        updatedGameState =
                            { gameState
                                | nobles =
                                    allNobles
                                        |> List.take noblesAmount
                            }
                    in
                    ( { model
                        | gameState = updatedGameState
                      }
                    , sendMessage (wrapEncoded (encodeGameState updatedGameState) "ingame")
                    )

                ( NotifyOnTurn notify, _ ) ->
                    ( { model | notifyOnPlayerTurn = notify }, Cmd.none )

                ( IncomingState received, _ ) ->
                    case D.decodeValue incomingDecoder received of
                        Ok result ->
                            case result of
                                GotGameState incomingGameState ->
                                    let
                                        isClientPlayerTurn =
                                            isActivePlayer clientPlayer incomingGameState.allPlayers

                                        buyTheColorsModal =
                                            if isClientPlayerTurn && model.notifyOnPlayerTurn then
                                                turnNotificationContent

                                            else
                                                model.buyTheColorsModal
                                    in
                                    ( { model
                                        | gameState = incomingGameState
                                        , buyTheColorsModal = buyTheColorsModal
                                      }
                                    , Cmd.none
                                    )

                                GotNotification notification ->
                                    ( { model | notificationMsg = notification.message }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        Err errorMsg ->
                            ( { model | notificationMsg = D.errorToString errorMsg }
                            , Cmd.none
                            )

                ( HideModal, _ ) ->
                    let
                        buyTheColorsModal =
                            model.buyTheColorsModal
                    in
                    ( { model | buyTheColorsModal = hideModal buyTheColorsModal }
                    , Cmd.none
                    )

                _ ->
                    ( { model | notificationMsg = "Unexpected msg condition found in CurrentGameModel" }, Cmd.none )

        Nothing ->
            ( { model | notificationMsg = "error with player data" }, Cmd.none )


updateCompletedGame : Msg -> CompletedGameModel -> ( CompletedGameModel, Cmd Msg )
updateCompletedGame msg model =
    case msg of
        HideModal ->
            let
                buyTheColorsModal =
                    model.buyTheColorsModal
            in
            ( { model | buyTheColorsModal = hideModal buyTheColorsModal }
            , Cmd.none
            )

        ShowCompletedGameModal ->
            ( { model | buyTheColorsModal = completedGameContent model.gameState.allPlayers }
            , Cmd.none
            )

        _ ->
            ( { model | notificationMsg = "Unexpected msg condition found in CompletedGameModel" }, Cmd.none )


updateReserveCard : CurrentGameModel -> Player -> BuyableCard -> PurchaseState -> ( CurrentGameModel, Cmd Msg )
updateReserveCard model clientPlayer buyableCard purchaseState =
    let
        gameState =
            model.gameState

        updatedPlayer =
            reserveCardFromPurchaseState purchaseState clientPlayer

        updatedGameState =
            updatePlayerInGameState updatedPlayer gameState
                |> replaceBuyableCard buyableCard
                |> incrementTurn
                |> logAction (clientPlayer.name ++ " reserved a card")
    in
    ( { model
        | buyTheColorsModal = hideModal model.buyTheColorsModal
        , gameState = updatedGameState
        , playerActionType = Nothing
      }
        |> clearNotificationMsg
        |> updateClientPlayer
    , sendMessage (wrapEncoded (encodeGameState updatedGameState) "ingame")
    )


reserveCardFromPurchaseState : PurchaseState -> Player -> Player
reserveCardFromPurchaseState purchaseState player =
    let
        cardToPurchase =
            getCardWithOriginalPrice purchaseState
    in
    -- Reserve the card
    addCardToPlayerReserve cardToPurchase.card player
        -- Add a Wild for reserving this card
        |> addWildToPlayer


getCardWithOriginalPrice : PurchaseState -> BuyableCard
getCardWithOriginalPrice purchaseState =
    let
        cardToPurchase =
            purchaseState.cardToPurchase

        card =
            cardToPurchase.card
    in
    { cardToPurchase
        | card =
            { card
                | price = purchaseState.originalCardPrice
            }
    }


wrapEncoded : E.Value -> String -> String
wrapEncoded val op =
    E.encode 0
        (E.object
            [ ( "op", E.string op )
            , ( "data", val )
            ]
        )


discardGem : GemColor -> DiscardGemsState -> DiscardGemsState
discardGem gemColor discardGemsState =
    { discardGemsState
        | gemsToKeep = Gems.minusOneGem gemColor discardGemsState.gemsToKeep
        , gemsToDiscard = Gems.plusOneGem gemColor discardGemsState.gemsToDiscard
    }


discardWild : DiscardGemsState -> DiscardGemsState
discardWild discardGemsState =
    let
        discardedWilds =
            discardGemsState.wildsToKeep
    in
    { discardGemsState | wildsToKeep = discardedWilds - 1 }


replaceBuyableCard : BuyableCard -> GameState -> GameState
replaceBuyableCard buyableCard gameState =
    case buyableCard.location of
        Just cardLocation ->
            let
                decks =
                    gameState.decks

                level =
                    buyableCard.card.level
            in
            case level of
                Level1 ->
                    let
                        ( maybeCard, replacedDeck ) =
                            drawOne gameState.decks.lvl1Deck

                        rowOfBuyableCards =
                            gameState.lvl1CardsDrawn
                    in
                    { gameState
                        | lvl1CardsDrawn = replaceCardInBuyableCards cardLocation maybeCard rowOfBuyableCards
                        , decks = { decks | lvl1Deck = replacedDeck }
                    }

                Level2 ->
                    let
                        ( maybeCard, replacedDeck ) =
                            drawOne gameState.decks.lvl2Deck

                        rowOfBuyableCards =
                            gameState.lvl2CardsDrawn
                    in
                    { gameState
                        | lvl2CardsDrawn = replaceCardInBuyableCards cardLocation maybeCard rowOfBuyableCards
                        , decks = { decks | lvl2Deck = replacedDeck }
                    }

                Level3 ->
                    let
                        ( maybeCard, replacedDeck ) =
                            drawOne gameState.decks.lvl3Deck

                        rowOfBuyableCards =
                            gameState.lvl3CardsDrawn
                    in
                    { gameState
                        | lvl3CardsDrawn = replaceCardInBuyableCards cardLocation maybeCard rowOfBuyableCards
                        , decks = { decks | lvl3Deck = replacedDeck }
                    }

        Nothing ->
            gameState


toCardPurchaseContent : Player -> PurchaseState -> BuyTheColorsModal
toCardPurchaseContent player purchaseState =
    cardPurchaseContent purchaseState.cardToPurchase player


payWithAGem : Payment -> PurchaseState -> PurchaseState
payWithAGem payment purchaseState =
    let
        updatedPrice =
            minusOneGem payment.gemColor purchaseState.cardToPurchase.card.price

        card =
            purchaseState.cardToPurchase.card

        updatedCard =
            { card | price = updatedPrice }

        cardToPurchase =
            purchaseState.cardToPurchase

        updatedCardToPurchase =
            { cardToPurchase
                | card = updatedCard
            }

        ( updatedPlayerGems, updatedPlayerWilds, updatedTableGems ) =
            let
                playerGems =
                    purchaseState.playerGems

                playerWilds =
                    purchaseState.playerWilds
            in
            if payment.paymentType == GemType then
                ( minusOneGem payment.gemColor playerGems
                , playerWilds
                , plusOneGem payment.gemColor purchaseState.tableGems
                )

            else
                ( playerGems
                , playerWilds - 1
                , purchaseState.tableGems
                )
    in
    PurchaseState updatedPlayerGems updatedTableGems updatedPlayerWilds updatedCardToPurchase purchaseState.originalCardPrice


completeCardPurchase : PurchaseState -> Player -> ( Player, TableGems )
completeCardPurchase purchaseState player =
    let
        purchasedCard =
            getCardWithOriginalPrice purchaseState
    in
    ( { player
        | gems = purchaseState.playerGems
        , wilds = purchaseState.playerWilds
      }
        |> addCardToPlayer purchasedCard.card
    , purchaseState.tableGems
    )


withNoCmd : CurrentGameModel -> ( CurrentGameModel, Cmd Msg )
withNoCmd model =
    ( model, Cmd.none )


checkForConfirm : CurrentGameModel -> CurrentGameModel
checkForConfirm model =
    case model.playerActionType of
        Just (GrabGemsType gems) ->
            case checkGrabGemType gems model.gameState.tableGems of
                Just gemActionType ->
                    case gemActionType of
                        TwoOfSameColor ->
                            { model
                                | buyTheColorsModal = confirmGemsContent gems "Take 2 of the same color"
                            }

                        ThreeOfDifferentColor ->
                            { model
                                | buyTheColorsModal = confirmGemsContent gems "Take 3 colors"
                            }

                        Available ->
                            { model
                                | buyTheColorsModal = confirmGemsContent gems "Take available colors"
                            }

                Nothing ->
                    model

        _ ->
            model


checkGrabGemType : List GemColor -> Gems -> Maybe GemActionType
checkGrabGemType gemsGrabbing tableGems =
    let
        gemGrabbingCount =
            List.length gemsGrabbing
    in
    if isGrabbingTwoOfSameColor gemsGrabbing then
        Just TwoOfSameColor

    else if gemGrabbingCount > 2 then
        Just ThreeOfDifferentColor

    else if isGrabbingRestOfAvailableGems tableGems gemsGrabbing then
        Just Available

    else
        -- No GemActionType, so not showing confirm modal
        Nothing


isGrabbingRestOfAvailableGems : TableGems -> List GemColor -> Bool
isGrabbingRestOfAvailableGems tableGems gemsGrabbing =
    let
        playerTakingOnlyOneGem =
            List.length gemsGrabbing == 1

        onlyGemsLeftAreSameColorAsThePlayerIsTaking =
            -- ListA (table) gems contain only gems in ListB (taking)
            -- The player can't take the gem colors they are already taking
            Gems.isSubListOf (Gems.toList tableGems) gemsGrabbing

        tableGemColorAllowsTwo =
            case List.head gemsGrabbing of
                Just gemColor ->
                    Gems.getGemAmount gemColor tableGems >= gemsAmountToAllowTwo

                Nothing ->
                    False
    in
    if onlyGemsLeftAreSameColorAsThePlayerIsTaking then
        let
            playerCanTakeAdditionalGemsThisTurn =
                playerTakingOnlyOneGem && tableGemColorAllowsTwo
        in
        if playerCanTakeAdditionalGemsThisTurn then
            False

        else
            True

    else
        False


confirmGemsContent : List GemColor -> String -> BuyTheColorsModal
confirmGemsContent gemsToGrab confirmText =
    { header = confirmText
    , content =
        [ Grid.row []
            [ Grid.col
                [ Col.xs12
                ]
                [ text "Take these colors?"
                ]
            ]
        , Grid.row []
            [ Grid.col
                [ Col.attrs
                    [ class "d-flex justify-content-center"
                    ]
                , Col.xs12
                ]
                (List.map displayGrabbedGem gemsToGrab
                    |> List.reverse
                )
            ]
        ]
    , footer =
        [ Button.button
            [ Button.onClick CancelGrabGem
            , Button.attrs
                [ class "btn btn-secondary"
                ]
            ]
            [ text "Cancel" ]
        , Button.button
            [ Button.onClick (ConfirmGrabGem gemsToGrab)
            , Button.attrs
                [ class "btn btn-primary"
                ]
            ]
            [ text "Confirm Action" ]
        ]
    , modalVisibility = Modal.shown
    , close = CancelGrabGem
    }


findClientPlayer : CurrentGameModel -> Maybe Player
findClientPlayer model =
    findPlayerByName model.clientPlayerName model.gameState.allPlayers


grabGem : GemColor -> CurrentGameModel -> CurrentGameModel
grabGem gemColor model =
    case model.playerActionType of
        -- Player is grabbing 1st gem
        Nothing ->
            { model
                | playerActionType = Just (GrabGemsType (List.singleton gemColor))
            }

        -- Player is already grabbing gems
        Just playerActionType ->
            case playerActionType of
                GrabGemsType gemsToGrab ->
                    { model
                        | playerActionType =
                            Just
                                (gemColor
                                    :: gemsToGrab
                                    |> GrabGemsType
                                )
                    }

                _ ->
                    model


incrementTurn : GameState -> GameState
incrementTurn gameState =
    { gameState
        | numberOfTurns = gameState.numberOfTurns + 1
        , allPlayers = rotateTurnOrder gameState.allPlayers
    }


rotateTurnOrder : List Player -> List Player
rotateTurnOrder players =
    let
        activePlayer =
            List.head players

        otherPlayers =
            List.drop 1 players
    in
    case activePlayer of
        Just player ->
            List.append otherPlayers [ player ]

        Nothing ->
            players


gemsAndWildCountExceeded : Gems -> Int -> Bool
gemsAndWildCountExceeded gems wilds =
    totalGemCount gems + wilds > playerMaxGems


view : Model -> Html Msg
view model =
    case model of
        Registering registerModel ->
            viewRegister registerModel

        Playing currentGame ->
            viewCurrentGame currentGame

        Completed completedGame ->
            viewCompletedGame completedGame


viewRegister : RegisterModel -> Html Msg
viewRegister register =
    Grid.containerFluid
        [ Flex.inline
        , Flex.col
        , Flex.alignItemsCenter
        ]
        [ CDN.stylesheet
        , div
            [ Spacing.mb3 ]
            [ img [ src (baseImgUrl ++ "buyTheColors-background.png") ] [] ]
        , if register.errorMessage /= "" then
            div []
                [ text register.errorMessage ]

          else
            div [] []
        , if register.notificationMessage /= "" then
            div []
                [ text register.notificationMessage ]

          else
            div [] []
        , div
            [ Spacing.mb3 ]
            [ input
                [ type_ "text"
                , onInput PlayerNameChanged
                , on "keydown" (ifIsEnter RegisterPlayer)
                , value register.myName
                , disabled (register.notificationMessage /= "")
                , placeholder "Player Name"
                , class "mr-1"
                ]
                []
            , Button.button
                [ Button.primary
                , Button.onClick RegisterPlayer
                , Button.disabled (register.notificationMessage /= "")
                ]
                [ text "Register"
                ]
            ]
        , div
            [ Spacing.mb3 ]
            (text "Players registered:"
                :: List.map
                    (\name ->
                        div [ class "text-center" ] [ text name ]
                    )
                    register.playerNames
            )
        , div
            [ Spacing.mb3 ]
            [ Button.button
                [ Button.primary
                , Button.onClick StartGame
                ]
                [ text "Start Game Now" ]
            ]
        ]


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg

                else
                    D.fail "some other key"
            )


viewCurrentGame : CurrentGameModel -> Html Msg
viewCurrentGame model =
    case findClientPlayer model of
        Just player ->
            case getActivePlayer model.gameState.allPlayers of
                Just activePlayer ->
                    let
                        gameState =
                            model.gameState

                        isActive =
                            isActivePlayer player gameState.allPlayers

                        turnNotificationToggle =
                            turnNotificationToggleContent model.notifyOnPlayerTurn

                        noblesTopRowContent =
                            noblesContent model.gameState.nobles

                        activePlayerContent =
                            activePlayerNameContent activePlayer.name
                    in
                    Grid.containerFluid []
                        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
                        , topRowContent turnNotificationToggle noblesTopRowContent activePlayerContent
                        , Grid.row
                            []
                            [ Grid.col
                                -- Table cards
                                [ Col.xs12
                                , Col.md10
                                ]
                                [ displayDeckAndBuyableCards isActive "card-back-lvl1.png" gameState.lvl1CardsDrawn
                                , displayDeckAndBuyableCards isActive "card-back-lvl2.png" gameState.lvl2CardsDrawn
                                , displayDeckAndBuyableCards isActive "card-back-lvl3.png" gameState.lvl3CardsDrawn
                                , displayClientPlayerArea player isActive
                                , div []
                                    (List.filter (\p -> p.name /= player.name) gameState.allPlayers
                                        |> List.map displayOtherPlayerArea
                                    )
                                ]
                            , Grid.col
                                -- Table gems
                                [ Col.xs12
                                , Col.md2
                                ]
                                (let
                                    maybeGems =
                                        model.playerActionType
                                            |> Maybe.andThen
                                                (\playerActionType ->
                                                    case playerActionType of
                                                        GrabGemsType grabbingGems ->
                                                            Just grabbingGems

                                                        _ ->
                                                            Nothing
                                                )
                                 in
                                 tableGemsArea isActive maybeGems gameState.tableGems
                                    ++ [ displayNotificationMsg model.notificationMsg ]
                                    ++ [ div []
                                            -- Clean up this UI using Bootstrap
                                            (List.map (\log -> div [] [ text log ]) gameState.gameLog)
                                       ]
                                )
                            ]
                        , modalView model.buyTheColorsModal
                        ]

                Nothing ->
                    text "Error with active player data"

        Nothing ->
            text "Error with player data"


viewCompletedGame : CompletedGameModel -> Html Msg
viewCompletedGame model =
    case findPlayerByName model.clientPlayerName model.gameState.allPlayers of
        Just player ->
            let
                gameState =
                    model.gameState

                gameCompleteText =
                    -- span [ onClick ShowCompletedGameModal ] [ text "Game Stats" ]
                    Button.button
                        [ Button.primary
                        , Button.onClick ShowCompletedGameModal
                        ]
                        [ text "Show End Game Stats" ]

                noblesTopRowContent =
                    noblesContent gameState.nobles

                winnerText =
                    case getPlayerNameWithMostPoints gameState.allPlayers of
                        Just winner ->
                            text <| winner ++ " won!"

                        Nothing ->
                            text "Error"
            in
            Grid.containerFluid []
                [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
                , topRowContent gameCompleteText noblesTopRowContent winnerText
                , Grid.row
                    []
                    [ Grid.col
                        -- Table cards
                        [ Col.xs12
                        , Col.md10
                        ]
                        [ displayDeckAndBuyableCards False "card-back-lvl1.png" gameState.lvl1CardsDrawn
                        , displayDeckAndBuyableCards False "card-back-lvl2.png" gameState.lvl2CardsDrawn
                        , displayDeckAndBuyableCards False "card-back-lvl3.png" gameState.lvl3CardsDrawn
                        , div [] [ text " ---- PLAYER AREA ---- " ]
                        , displayClientPlayerArea player False
                        , div [] [ text " ---- OTHER PLAYERS AREA ---- " ]
                        , div []
                            (List.filter (\p -> p.name /= player.name) gameState.allPlayers
                                |> List.map displayOtherPlayerArea
                            )
                        ]
                    , Grid.col
                        -- Table gems
                        [ Col.xs12
                        , Col.md2
                        ]
                        (tableGemsArea False Nothing gameState.tableGems
                            ++ [ displayNotificationMsg model.notificationMsg ]
                            ++ [ div []
                                    -- Clean up this UI using Bootstrap
                                    (List.map (\log -> div [] [ text log ]) gameState.gameLog)
                               ]
                        )
                    ]
                , modalView model.buyTheColorsModal
                ]

        Nothing ->
            text "Error with player data"


displayNotificationMsg : String -> Html Msg
displayNotificationMsg message =
    div [ style "margin" "5px 0" ] [ text message ]


displayClientPlayerArea : Player -> Bool -> Html Msg
displayClientPlayerArea player isActive =
    let
        displayReservedCard =
            if isActive then
                displayBuyableReservedCard

            else
                displayViewableReservedCard
    in
    displayPlayerArea displayReservedCard player


displayOtherPlayerArea : Player -> Html Msg
displayOtherPlayerArea player =
    displayPlayerArea displayHiddenReservedCard player


displayPlayerArea : (Card -> Grid.Column Msg) -> Player -> Html Msg
displayPlayerArea displayReservedCard player =
    div [ class "mb-3" ]
        [ div [ style "font-weight" "bold" ] [ text ("Name: " ++ player.name) ]
        , div [] [ text ("Colors: " ++ Gems.concatGemAmount player.gems) ]
        , div []
            [ "Wilds: "
                ++ (player.wilds
                        |> String.fromInt
                   )
                |> text
            ]
        , div [] [ text "Purchased Cards" ]
        , displayMyPurchasedCards player.cards
        , div [] [ text "Reserved Cards" ]
        , Grid.row []
            [ Grid.col [ Col.xs4 ]
                [ Grid.row
                    [ Row.attrs []
                    ]
                    (List.map displayReservedCard player.reservedCards)
                ]
            ]
        , div [] [ text "Noble visits" ]
        , Grid.row []
            [ Grid.col [ Col.xs4 ]
                [ Grid.row
                    [ Row.attrs []
                    ]
                    (List.map displayPlayerNoble player.noblesVisited)
                ]
            ]
        , div []
            [ "Victory Points: "
                ++ (victoryPoints player
                        |> String.fromInt
                   )
                |> text
            ]
        ]


displayViewableReservedCard : Card -> Grid.Column msg
displayViewableReservedCard card =
    Grid.col
        [ Col.xs4
        , Col.attrs
            [ Spacing.px1
            ]
        ]
        [ Html.img
            (getCardImgAttributes card.url)
            []
        ]


displayHiddenReservedCard : Card -> Grid.Column Msg
displayHiddenReservedCard card =
    Grid.col
        [ Col.xs4
        , Col.attrs
            [ Spacing.px1
            ]
        ]
        [ Html.img
            [ src (backOfCardImgUrl card.level)
            , style "width" "100%"
            , style "border-radius" "10px"
            ]
            []
        ]


displayPlayerNoble : Noble -> Grid.Column msg
displayPlayerNoble noble =
    Grid.col
        [ Col.xs4
        , Col.attrs
            [ Spacing.px1
            ]
        ]
        [ img
            [ src (toImageUrl noble.url)
            , style "border-radius" "10px"
            ]
            []
        ]


backOfCardImgUrl : DeckLevel -> String
backOfCardImgUrl level =
    baseImgUrl
        ++ (case level of
                Level1 ->
                    "card-back-lvl1.png"

                Level2 ->
                    "card-back-lvl2.png"

                Level3 ->
                    "card-back-lvl3.png"
           )


displayMyPurchasedCards : List Card -> Html Msg
displayMyPurchasedCards cards =
    let
        sortedCards =
            sortByDiscount cards
    in
    Grid.row []
        [ Grid.col [ Col.xs4 ]
            [ Grid.row
                [ Row.attrs []
                ]
                [ Grid.col
                    [ Col.xs4
                    , Col.attrs
                        [ Spacing.px1
                        ]
                    ]
                    (List.indexedMap displayMyPurchasedCard sortedCards)
                ]
            ]
        ]


displayMyPurchasedCard : Int -> Card -> Html msg
displayMyPurchasedCard idx card =
    let
        marginTopAttr =
            style "margin-top" "-90px"

        zIndexAttr =
            style "z-index" (String.fromInt idx)

        overlapAttrs =
            if idx > 0 then
                [ marginTopAttr, zIndexAttr ]

            else
                [ zIndexAttr ]
    in
    Html.img
        (getCardImgAttributes card.url ++ overlapAttrs)
        []


isGrabbingTwoOfSameColor : List GemColor -> Bool
isGrabbingTwoOfSameColor gems =
    if List.length gems > 1 then
        let
            firstTwo =
                List.take 2 gems

            firstColor =
                List.head firstTwo
        in
        case firstColor of
            Just color ->
                case List.foldl foldBySameColor ( True, color ) firstTwo of
                    ( True, _ ) ->
                        True

                    ( False, _ ) ->
                        False

            Nothing ->
                False

    else
        False


foldBySameColor : GemColor -> ( Bool, GemColor ) -> ( Bool, GemColor )
foldBySameColor gemColor ( isSame, accGemColor ) =
    if isSame && gemColor == accGemColor then
        ( True, accGemColor )

    else
        ( False, accGemColor )



-- This is actually 4 for any number of players


gemsAmountToAllowTwo : Int
gemsAmountToAllowTwo =
    4


canGrabGem : Maybe (List GemColor) -> Gems -> GemColor -> Bool
canGrabGem gemsGrabbing tableGems gemToGrab =
    let
        tableGemColorCount =
            getGemAmount gemToGrab tableGems
    in
    if tableGemColorCount > 0 then
        case gemsGrabbing of
            Just gems ->
                case gems of
                    [] ->
                        -- I'm not currently grabbing any other gems, so allow it
                        True

                    [ gemColor ] ->
                        -- If the gemToGrab is the same color as the ONE gem I'm currently grabbing, I can grab it as
                        -- long as there's 4 or more of that gem on the table (this is the "take 2" action).
                        let
                            isGrabbingDifferentColorGem =
                                not (gemToGrab == gemColor)

                            doesTableHaveRequiredAmount =
                                tableGemColorCount >= gemsAmountToAllowTwo
                        in
                        if isGrabbingDifferentColorGem then
                            True

                        else
                            doesTableHaveRequiredAmount

                    multipleGems ->
                        -- Allow grabbing if the gemToGrab is a different color than the ones I'm already grabbing
                        not (List.member gemToGrab multipleGems)

            Nothing ->
                True

    else
        False


noblesContent : List Noble -> Html Msg
noblesContent nobles =
    Grid.row
        [ Row.attrs
            [ Flex.justifyCenter
            ]
        ]
        (List.map
            (\noble ->
                Grid.col
                    [ Col.xs3
                    , Col.md2
                    ]
                    [ img [ src (toImageUrl noble.url)
                          , width 120
                          ] [] ]
            )
            nobles
        )


turnNotificationToggleContent : Bool -> Html Msg
turnNotificationToggleContent notifyMe =
    Grid.row
        [ Row.attrs
            [ class "d-flex"
            , Flex.justifyCenter
            , Flex.alignItemsCenter
            ]
        ]
        [ Grid.col
            [ Col.xs8 ]
            [ text "Notify me when it's my turn" ]
        , Grid.col
            [ Col.xs4 ]
            [ label
                [ class "switch" ]
                [ input
                    [ type_ "checkbox"
                    , checked notifyMe
                    , onClick <| NotifyOnTurn <| not notifyMe
                    ]
                    []
                , span [ class "slider round" ] []
                ]
            ]
        ]


activePlayerNameContent : String -> Html Msg
activePlayerNameContent name =
    text <| "It's " ++ name ++ "'s turn"


topRowContent : Html Msg -> Html Msg -> Html Msg -> Html Msg
topRowContent leftContent innerContent rightContent =
    Grid.row
        []
        -- 1st col: col-2 d-flex align-items-end justify-content-center
        [ Grid.col
            [ Col.xs2
            , Col.attrs
                [ Flex.alignItemsCenter
                , Flex.justifyCenter
                , class "d-flex"
                ]
            ]
            [ leftContent ]
        , Grid.col
            [ Col.xs8 ]
            [ innerContent ]
        , Grid.col
            [ Col.xs2
            , Col.attrs
                [ Flex.alignItemsCenter
                , Flex.justifyCenter
                , class "d-flex"
                ]
            ]
            [ rightContent ]
        ]


cardImgWidth : String
cardImgWidth =
    -- "100%" my monitor
    -- "65%" my laptop screen
    "65%"



-- Refactor to pass in the DeckLevel instead of the deck img name


displayDeckAndBuyableCards : Bool -> String -> RowOfBuyableCards -> Html Msg
displayDeckAndBuyableCards isActive deckImgName buyableCards =
    let
        displayCard =
            displayCardOrEmpty isActive
    in
    Grid.row
        [ Row.attrs
            [ Flex.justifyBetween
            , Spacing.my1
            ]
        ]
        -- Display deck back and buyable cards
        [ Grid.col
            [ Col.xs2
            , Col.attrs
                [ class "deck" ]
            ]
            [ img
                (getCardImgAttributes deckImgName)
                []
            ]
        , Grid.col
            [ Col.xs2
            ]
            -- Left card
            [ displayCard buyableCards.left LeftCard
            ]
        , Grid.col
            [ Col.xs2
            ]
            -- Middle Left
            [ displayCard buyableCards.middleLeft MiddleLeftCard
            ]
        , Grid.col
            [ Col.xs2
            ]
            -- Middle Right
            [ displayCard buyableCards.middleRight MiddleRightCard
            ]
        , Grid.col
            [ Col.xs2
            ]
            -- Right
            [ displayCard buyableCards.right RightCard
            ]
        ]


cardImgAttrsFromBuyableCard : BuyableCard -> List (Attribute Msg)
cardImgAttrsFromBuyableCard buyableCard =
    (buyableCard
        |> onClickBuyableCard
    )
        :: getCardImgAttributes buyableCard.card.url


displayBuyableReservedCard : Card -> Grid.Column Msg
displayBuyableReservedCard card =
    let
        cardImgAttrs =
            cardImgAttrsFromBuyableCard (BuyableCard card Nothing False)
    in
    Grid.col
        [ Col.xs4
        , Col.attrs
            [ Spacing.px1
            ]
        ]
        [ img cardImgAttrs [] ]


displayCardOrEmpty : Bool -> Maybe Card -> BuyableCardLocation -> Html Msg
displayCardOrEmpty isActive maybeCard location =
    case maybeCard of
        Just card ->
            let
                cardImgAttrs =
                    if isActive then
                        cardImgAttrsFromBuyableCard (BuyableCard card (Just location) True)

                    else
                        getCardImgAttributes card.url
            in
            img cardImgAttrs []

        Nothing ->
            span [] []


onClickBuyableCard : BuyableCard -> Attribute Msg
onClickBuyableCard buyableCard =
    ViewCardModal buyableCard
        |> onClick


modalView : BuyTheColorsModal -> Html Msg
modalView buyTheColorsModal =
    Modal.config buyTheColorsModal.close
        |> Modal.h5 [] [ text buyTheColorsModal.header ]
        |> Modal.body []
            [ Grid.containerFluid []
                buyTheColorsModal.content
            ]
        |> Modal.footer []
            buyTheColorsModal.footer
        |> Modal.view buyTheColorsModal.modalVisibility


isPaymentMade : List GemColor -> Card -> Bool
isPaymentMade discounts card =
    let
        price =
            card.price

        discountsFor =
            sumByGemColor discounts
    in
    isPaid (price.red - discountsFor Red)
        && isPaid (price.blue - discountsFor Blue)
        && isPaid (price.green - discountsFor Green)
        && isPaid (price.white - discountsFor White)
        && isPaid (price.black - discountsFor Black)


sumByGemColor : List GemColor -> GemColor -> Int
sumByGemColor gems gemColor =
    List.filter (\gem -> gem == gemColor) gems
        |> List.length


isPaid : Int -> Bool
isPaid amount =
    amount <= 0


discardGemsContent : DiscardGemsState -> BuyTheColorsModal
discardGemsContent discardGemsState =
    let
        header =
            "Discard down to " ++ String.fromInt playerMaxGems ++ " gems"

        gems =
            discardGemsState.gemsToKeep

        totalGems =
            totalGemCount discardGemsState.gemsToKeep + discardGemsState.wildsToKeep

        isMinAmountReached =
            totalGems <= playerMaxGems

        discardGemContent =
            \gemColor count ->
                Button.button
                    [ getButtonOption gemColor
                    , DiscardedGem (DiscardedGemType gemColor)
                        |> Button.onClick
                    , Button.attrs paymentBtnAttrs
                    , Button.disabled (isMinAmountReached || count <= 0)
                    ]
                    [ text ("Discard " ++ Gems.toString gemColor ++ " gem (You have " ++ String.fromInt count ++ ")") ]

        discardWildContent =
            \count ->
                Button.button
                    [ Button.warning
                    , DiscardedGem DiscardedWildType
                        |> Button.onClick
                    , Button.attrs paymentBtnAttrs
                    , Button.disabled (isMinAmountReached || count <= 0)
                    ]
                    [ text ("Discard Wild (" ++ String.fromInt count ++ ") gems") ]

        isConfirmDisabled =
            gemsAndWildCountExceeded discardGemsState.gemsToKeep discardGemsState.wildsToKeep

        content =
            [ Grid.row
                []
                [ Grid.col
                    [ Col.xs5 ]
                    [ text ("You have " ++ String.fromInt totalGems) ]
                ]
            , Grid.row
                [ Row.attrs
                    [ Flex.justifyAround ]
                ]
                [ Grid.col
                    [ Col.xs5 ]
                    [ discardGemContent Red gems.red
                    ]
                , Grid.col
                    [ Col.xs5 ]
                    [ discardGemContent Blue gems.blue
                    ]
                , Grid.col
                    [ Col.xs5 ]
                    [ discardGemContent Green gems.green
                    ]
                , Grid.col
                    [ Col.xs5 ]
                    [ discardGemContent White gems.white
                    ]
                , Grid.col
                    [ Col.xs5 ]
                    [ discardGemContent Black gems.black
                    ]
                , Grid.col
                    [ Col.xs5 ]
                    [ discardWildContent discardGemsState.wildsToKeep
                    ]
                ]
            ]

        footer =
            [ Button.button
                [ Button.attrs
                    [ class "btn btn-secondary"
                    ]
                , Button.onClick ResetDiscardedGems
                ]
                [ text "Reset" ]
            , Button.button
                [ Button.attrs
                    [ class "btn btn-primary"
                    ]
                , Button.onClick ConfirmDiscardedGems
                , Button.disabled isConfirmDisabled
                ]
                [ text "Confirm" ]
            ]
    in
    BuyTheColorsModal header content footer Modal.shown ResetDiscardedGems


cardPurchaseContent : BuyableCard -> Player -> BuyTheColorsModal
cardPurchaseContent buyableCard player =
    let
        card =
            buyableCard.card

        header =
            "Purchase Card"

        canReserve =
            List.length player.reservedCards < 3

        content =
            [ Grid.row
                [ Row.attrs
                    [ Flex.alignItemsCenter
                    , Flex.justifyCenter
                    ]
                ]
                [ Grid.col
                    [ Col.xs3
                    , Col.lg6
                    ]
                    [ img (getCardImgAttributes card.url) [] ]
                , Grid.col
                    [ Col.xs6
                    , Col.attrs [ class "pay-with" ]
                    ]
                    [ Grid.row
                        [ Row.attrs
                            [ style "margin-bottom" "1em"
                            ]
                        ]
                        [ Grid.col []
                            [ text "Pay with your colors or wilds"
                            ]
                        ]
                    , cardPurchaseButton player card White
                    , cardPurchaseButton player card Blue
                    , cardPurchaseButton player card Green
                    , cardPurchaseButton player card Red
                    , cardPurchaseButton player card Black
                    ]
                ]
            , Grid.row
                []
                (if buyableCard.isReservable then
                    [ Grid.col
                        [ Col.attrs
                            [ Flex.inline
                            , Flex.alignItemsCenter
                            , Flex.justifyCenter
                            , Spacing.mt3
                            ]
                        ]
                        [ Button.button
                            [ Button.warning
                            , Button.onClick (ReserveCard buyableCard)
                            , Button.disabled (not canReserve)
                            ]
                            [ text "Reserve Card"
                            ]
                        , span
                            [ style "margin-left" "5px"
                            ]
                            [ text "And get 1 Wild"
                            ]
                        ]
                    ]

                 else
                    []
                )
            ]

        footer =
            let
                playerDiscounts =
                    Player.discounts player

                confirmEnabled =
                    isPaymentMade playerDiscounts card
            in
            [ Button.button
                [ Button.onClick ResetPurchase
                , Button.attrs
                    [ class "btn btn-secondary"
                    ]
                ]
                [ text "Cancel" ]
            , Button.button
                [ Button.onClick (ConfirmCardPurchase buyableCard)
                , Button.disabled (not confirmEnabled)
                , Button.attrs
                    [ class "btn btn-primary"
                    ]
                ]
                [ text "Purchase Card" ]
            ]
    in
    BuyTheColorsModal header content footer Modal.shown ResetPurchase


cardPurchaseButton : Player -> Card -> GemColor -> Html Msg
cardPurchaseButton player card gemColor =
    let
        isAmtRemaining =
            let
                cardGemAmount =
                    Gems.getGemAmount gemColor card.price

                playerDiscounts =
                    discounts player
                        |> List.filter (\gem -> gem == gemColor)
                        |> List.length
            in
            (cardGemAmount - playerDiscounts) > 0

        buttonEnabled =
            let
                playerGemAmount =
                    Gems.getGemAmount gemColor player.gems
            in
            playerGemAmount > 0 && isAmtRemaining

        wildEnabled =
            player.wilds > 0 && isAmtRemaining
    in
    Grid.row
        [ Row.attrs
            [ Flex.justifyAround
            ]
        ]
        [ Grid.col
            [ Col.xs5
            ]
            [ Button.button
                [ getButtonOption gemColor
                , Button.disabled (not buttonEnabled)
                , Button.onClick
                    (Payment GemType gemColor
                        |> PayForCard
                    )
                , Button.attrs paymentBtnAttrs
                ]
                [ text (Gems.toString gemColor) ]
            ]
        , Grid.col
            [ Col.xs5
            ]
            [ Button.button
                [ Button.warning
                , Button.disabled (not wildEnabled)
                , Button.onClick
                    (Payment WildType gemColor
                        |> PayForCard
                    )
                , Button.attrs paymentBtnAttrs
                ]
                [ text "Wild" ]
            ]
        ]


turnNotificationContent : BuyTheColorsModal
turnNotificationContent =
    let
        header =
            "Notification"

        content =
            [ Grid.row
                [ Row.attrs
                    [ Flex.justifyCenter
                    ]
                ]
                [ Grid.col
                    [ Col.xs12
                    ]
                    [ text "It's your turn" ]
                ]
            ]

        footer =
            [ Button.button
                [ Button.onClick HideModal
                , Button.attrs
                    [ class "btn btn-primary"
                    ]
                ]
                [ text "Ok" ]
            ]
    in
    BuyTheColorsModal header content footer Modal.shown HideModal


completedGameContent : List Player -> BuyTheColorsModal
completedGameContent players =
    let
        header =
            "Victory!"

        content =
            [ Grid.row
                [ Row.attrs
                    [ Flex.alignItemsCenter
                    , Flex.justifyCenter
                    ]
                ]
                [ Grid.col
                    [ Col.xs12
                    ]
                    (Grid.row
                        []
                        [ Grid.col []
                            [ text "Player " ]
                        , Grid.col []
                            [ text "VP" ]
                        , Grid.col []
                            [ text "Purchased Cards" ]
                        , Grid.col []
                            [ text "Noble Visits" ]
                        ]
                        :: (List.sortBy (\player -> Player.victoryPoints player) players
                                |> List.reverse
                                |> List.map endGamePlayerContent
                           )
                    )
                ]
            ]

        footer =
            []
    in
    BuyTheColorsModal header content footer Modal.shown HideModal


endGamePlayerContent : Player -> Html msg
endGamePlayerContent player =
    Grid.row
        []
        [ Grid.col
            []
            [ text player.name ]
        , Grid.col
            []
            [ Player.victoryPoints player
                |> String.fromInt
                |> text
            ]
        , Grid.col
            []
            [ List.length player.cards
                |> String.fromInt
                |> text
            ]
        , Grid.col
            []
            [ List.length player.noblesVisited
                |> String.fromInt
                |> text
            ]
        ]


getCardImgAttributes : String -> List (Attribute msg)
getCardImgAttributes imgUrl =
    [ src (toImageUrl imgUrl)
    , style "width" cardImgWidth
    , style "border-radius" "10px"
    ]


paymentBtnAttrs : List (Attribute msg)
paymentBtnAttrs =
    [ style "width" "5em" ]


hideModal : BuyTheColorsModal -> BuyTheColorsModal
hideModal modal =
    { modal | modalVisibility = Modal.hidden }


logAction : String -> GameState -> GameState
logAction descr gameState =
    { gameState | gameLog = descr :: gameState.gameLog }


describeGrabGemAction : String -> List GemColor -> String
describeGrabGemAction name gems =
    if isGrabbingTwoOfSameColor gems then
        case List.head gems of
            Just gemColor ->
                name ++ " took 2 " ++ toString gemColor ++ " colors"

            Nothing ->
                ""

    else
        let
            gemText =
                if List.length gems == 1 then
                    "color"

                else
                    "colors"
        in
        name
            ++ " took "
            ++ String.fromInt (List.length gems)
            ++ " "
            ++ gemText
            ++ " ("
            ++ (List.map toString gems
                    |> String.join ", "
               )
            ++ ")"


baseImgUrl : String
baseImgUrl =
    "/images/"


toImageUrl : String -> String
toImageUrl url =
    baseImgUrl ++ url



-- Taking Gems


toGemFileName : GemColor -> String
toGemFileName gem =
    (toString gem
        |> String.toLower
    )
        ++ ".png"


tableGemsContent : Bool -> Maybe (List GemColor) -> Gems -> Html Msg
tableGemsContent isActive gemsGrabbing gems =
    let
        isButtonEnabled =
            canGrabGem gemsGrabbing gems

        buttonOptions =
            gemButtonOptions isActive

        redOptions =
            buttonOptions Red isButtonEnabled

        blueOptions =
            buttonOptions Blue isButtonEnabled

        greenOptions =
            buttonOptions Green isButtonEnabled

        blackOptions =
            buttonOptions Black isButtonEnabled

        whiteOptions =
            buttonOptions White isButtonEnabled
    in
    Grid.row
        [ Row.attrs
            [ Flex.alignItemsCenter
            , Flex.justifyAround
            , class "table-gem"
            ]
        ]
        [ displayTableGem gems.red redOptions
        , displayTableGem gems.blue blueOptions
        , displayTableGem gems.green greenOptions
        , displayTableGem gems.black blackOptions
        , displayTableGem gems.white whiteOptions
        ]


tableGemsArea : Bool -> Maybe (List GemColor) -> Gems -> List (Html Msg)
tableGemsArea isActive gemsToGrab tableGems =
    let
        gemsContent =
            [ Grid.row
                []
                [ Grid.col []
                    [ text "Take 2 or 3 Colors" ]
                ]
            , tableGemsContent isActive gemsToGrab tableGems
            ]
    in
    case gemsToGrab of
        Just gems ->
            gemsContent ++ [ displayResetGems gems ]

        Nothing ->
            gemsContent


displayGrabbedGem : GemColor -> Html msg
displayGrabbedGem gem =
    let
        gemColorText =
            toString gem

        bgColor =
            if gem == White then
                "#f2f2f2"

            else
                gemColorText

        color =
            if gem == White then
                "black"

            else
                "white"
    in
    span
        [ class "m-2"
        , style "background-color" bgColor
        , style "padding" "5px"
        , style "color" color
        , style "border-radius" "3px"
        ]
        [ text gemColorText ]


displayResetGems : List GemColor -> Html Msg
displayResetGems gems =
    Grid.row []
        [ Grid.col
            [ Col.attrs
                [ class "col d-flex justify-content-center"
                ]
            , Col.xs12
            ]
            (List.map displayGrabbedGem gems
                |> List.reverse
            )
        , Grid.col
            [ Col.attrs
                [ class "d-flex justify-content-center mt-2"
                ]
            , Col.xs12
            ]
            [ Button.button
                [ Button.onClick CancelGrabGem
                , Button.attrs
                    [ class "btn btn-secondary mt-2 w-75"
                    ]
                ]
                [ text "Reset Gems" ]
            ]
        ]


displayTableGem : Int -> List (Button.Option Msg) -> Grid.Column Msg
displayTableGem count buttonOptions =
    Grid.col
        [ Col.xs4
        , Col.md12
        ]
        [ Grid.row []
            [ Grid.col
                [ Col.attrs
                    [ class "table-gem-token"
                    ]
                , Col.md6
                ]
                [ span []
                    [ let
                        gemCountText =
                            if count == 1 then
                                "color"

                            else
                                "colors"
                      in
                      "("
                        ++ String.fromInt count
                        ++ " "
                        ++ gemCountText
                        ++ ")"
                        |> text
                    ]
                ]
            , Grid.col
                [ Col.md4
                ]
                [ Button.button
                    buttonOptions
                    [ text "Take Color" ]
                ]
            ]
        ]


gemButtonOptions : Bool -> GemColor -> (GemColor -> Bool) -> List (Button.Option Msg)
gemButtonOptions isActivePlayer gemColor enabled =
    let
        isEnabled =
            enabled gemColor

        options =
            [ getButtonOption gemColor
            , Button.disabled (not isEnabled)
            ]
    in
    if isActivePlayer then
        (GrabGem gemColor
            |> Button.onClick
        )
            :: options

    else
        options


tempTableGemImgHeightStyle : Attribute msg
tempTableGemImgHeightStyle =
    style "height" "auto"


tempTableGemImgWidthStyle : Attribute msg
tempTableGemImgWidthStyle =
    style "width" "80%"


getButtonOption : GemColor -> Button.Option msg
getButtonOption gem =
    case gem of
        Red ->
            Button.danger

        Blue ->
            Button.primary

        Green ->
            Button.success

        White ->
            Button.light

        Black ->
            Button.dark


incomingDecoder : D.Decoder IncomingType
incomingDecoder =
    D.oneOf
        [ D.map GotGameState gameStateDecoder
        , D.map GotNotification notificationDecoder
        , D.map GotStartGame (D.list D.string)
        , D.map GotNewlyRegisteredPlayer newlyRegisteredPlayerDecoder
        , D.map GotRegisteredPlayers registeredPlayersDecoder
        ]


notificationDecoder : D.Decoder Notification
notificationDecoder =
    D.succeed Notification
        |> DE.andMap (D.field "title" D.string)
        |> DE.andMap (D.field "message" D.string)


newlyRegisteredPlayerDecoder : D.Decoder String
newlyRegisteredPlayerDecoder =
    D.field "newRegisteredPlayer" D.string


registeredPlayersDecoder : D.Decoder (List String)
registeredPlayersDecoder =
    D.field "registeredPlayers" (D.list D.string)
