module Gems exposing (..)

import Json.Decode as D exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Encode as E exposing (..)


type alias Gems =
    { red : Int
    , blue : Int
    , green : Int
    , white : Int
    , black : Int
    }


type GemColor
    = Red
    | Blue
    | Green
    | White
    | Black


initEmptyGems : Gems
initEmptyGems =
    Gems 0 0 0 0 0


concatGemAmount : Gems -> String
concatGemAmount gems =
    "Red: "
        ++ showAmount Red gems
        ++ " | Blue: "
        ++ showAmount Blue gems
        ++ " | Green: "
        ++ showAmount Green gems
        ++ " | Black: "
        ++ showAmount Black gems
        ++ " | White: "
        ++ showAmount White gems


showAmount : GemColor -> Gems -> String
showAmount color gems =
    String.fromInt (getGemAmount color gems)



-- Why did I not just name this subtractOne and addOne??


minusOneGem : GemColor -> Gems -> Gems
minusOneGem gemColor gems =
    updateGems (\amt -> amt - 1) gemColor gems


plusOneGem : GemColor -> Gems -> Gems
plusOneGem gemColor gems =
    updateGems (\amt -> amt + 1) gemColor gems


addAll : Gems -> Gems -> Gems
addAll gemsA gemsB =
    Gems (gemsA.red + gemsB.red)
        (gemsA.blue + gemsB.blue)
        (gemsA.green + gemsB.green)
        (gemsA.white + gemsB.white)
        (gemsA.black + gemsB.black)


updateGems : (Int -> Int) -> GemColor -> Gems -> Gems
updateGems transform gemColor gems =
    let
        newVal =
            transform (getGemAmount gemColor gems)
    in
    setGemAmount gemColor newVal gems


totalGemCount : Gems -> Int
totalGemCount gems =
    gems.red
        + gems.blue
        + gems.green
        + gems.white
        + gems.black


toString : GemColor -> String
toString gemColor =
    case gemColor of
        Red ->
            "Red"

        Blue ->
            "Blue"

        Green ->
            "Green"

        White ->
            "White"

        Black ->
            "Black"


fromString : String -> Maybe GemColor
fromString name =
    if name == "Red" then
        Just Red

    else if name == "Blue" then
        Just Blue

    else if name == "Green" then
        Just Green

    else if name == "White" then
        Just White

    else if name == "Black" then
        Just Black

    else
        Nothing


toList : Gems -> List GemColor
toList gems =
    List.repeat gems.red Red
        ++ List.repeat gems.blue Blue
        ++ List.repeat gems.green Green
        ++ List.repeat gems.white White
        ++ List.repeat gems.black Black


isSubListOf : List GemColor -> List GemColor -> Bool
isSubListOf listA listB =
    -- returns True if A a sublist of B
    let
        eachMember =
            \gemColor -> List.member gemColor listB
    in
    List.all eachMember listA


getGemAmount : GemColor -> Gems -> Int
getGemAmount gemColor gems =
    case gemColor of
        Red ->
            gems.red

        Blue ->
            gems.blue

        Green ->
            gems.green

        White ->
            gems.white

        Black ->
            gems.black


setGemAmount : GemColor -> Int -> Gems -> Gems
setGemAmount gemColor value gems =
    case gemColor of
        Red ->
            { gems | red = value }

        Blue ->
            { gems | blue = value }

        Green ->
            { gems | green = value }

        White ->
            { gems | white = value }

        Black ->
            { gems | black = value }


gemColorDecoder : D.Decoder GemColor
gemColorDecoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just gemColor ->
                        succeed gemColor

                    Nothing ->
                        D.fail ("unknown GemColor: " ++ str)
            )


gemsEquals : Gems -> Gems -> Bool
gemsEquals gemsA gemsB =
    gemsA.red
        == gemsB.red
        && gemsA.blue
        == gemsB.blue
        && gemsA.green
        == gemsB.green
        && gemsA.white
        == gemsB.white
        && gemsA.black
        == gemsB.black


encodeGems : Gems -> E.Value
encodeGems gems =
    E.object
        [ ( "red", E.int gems.red )
        , ( "blue", E.int gems.blue )
        , ( "green", E.int gems.green )
        , ( "white", E.int gems.white )
        , ( "black", E.int gems.black )
        ]


gemsDecoder : D.Decoder Gems
gemsDecoder =
    succeed Gems
        |> andMap (field "red" D.int)
        |> andMap (field "blue" D.int)
        |> andMap (field "green" D.int)
        |> andMap (field "white" D.int)
        |> andMap (field "black" D.int)
