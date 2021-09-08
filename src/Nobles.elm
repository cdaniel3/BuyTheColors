module Nobles exposing (..)

import Cards exposing (..)
import Gems exposing (..)
import Json.Decode as D exposing (..)
import Json.Decode.Extra as DE exposing (..)
import Json.Encode as E exposing (..)
import Json.Encode.Extra exposing (..)


allNobles : List Noble
allNobles =
    [ noble4B4G3Points, noble3R3B3G3Points, noble4R4G3Points, noble3B3W3B, noble4Black4W, noble4Blue4W, noble3R3B3W, noble3R3G3B, noble4R4B, noble3G3B3W ]


noble4R4B : Noble
noble4R4B =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Red 4
                |> setGemAmount Black 4
    in
    Noble visitAmount 3 "bonus/bonus_40004_3.png"


noble3R3G3B : Noble
noble3R3G3B =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Red 3
                |> setGemAmount Black 3
                |> setGemAmount Green 3
    in
    Noble visitAmount 3 "bonus/bonus_30303_3.png"


noble3R3B3W : Noble
noble3R3B3W =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Red 3
                |> setGemAmount Black 3
                |> setGemAmount White 3
    in
    Noble visitAmount 3 "bonus/bonus_30033_3.png"


noble4Blue4W : Noble
noble4Blue4W =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Blue 4
                |> setGemAmount White 4
    in
    Noble visitAmount 3 "bonus/bonus_04040_3.png"


noble4Black4W : Noble
noble4Black4W =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Black 4
                |> setGemAmount White 4
    in
    Noble visitAmount 3 "bonus/bonus_00044_3.png"


noble3B3W3B : Noble
noble3B3W3B =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Blue 3
                |> setGemAmount Black 3
                |> setGemAmount White 3
    in
    Noble visitAmount 3 "bonus/bonus_03033_3.png"


nobleGBOnePoint : Noble
nobleGBOnePoint =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Blue 1
                |> setGemAmount Green 1
    in
    Noble visitAmount 1 "bonus/noble_04400_3.png"


noble4B4G3Points : Noble
noble4B4G3Points =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Blue 4
                |> setGemAmount Green 4
    in
    Noble visitAmount 3 "bonus/bonus_04400_3.png"


noble3R3B3G3Points : Noble
noble3R3B3G3Points =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Red 3
                |> setGemAmount Blue 3
                |> setGemAmount Green 3
    in
    Noble visitAmount 3 "bonus/bonus_33300_3.png"


noble4R4G3Points : Noble
noble4R4G3Points =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Red 4
                |> setGemAmount Green 4
    in
    Noble visitAmount 3 "bonus/bonus_40400_3.png"


noble3G3B3W : Noble
noble3G3B3W =
    let
        visitAmount =
            initEmptyGems
                |> setGemAmount Blue 3
                |> setGemAmount Green 3
                |> setGemAmount White 3
    in
    Noble visitAmount 3 "bonus/bonus_03330_3.png"


encodeNoble : Noble -> E.Value
encodeNoble noble =
    E.object
        [ ( "visitAmount", encodeGems noble.visitAmount )
        , ( "victoryPoints", E.int noble.victoryPoints )
        , ( "url", E.string noble.url )
        ]


nobleDecoder : D.Decoder Noble
nobleDecoder =
    succeed Noble
        |> DE.andMap (field "visitAmount" gemsDecoder)
        |> DE.andMap (field "victoryPoints" D.int)
        |> DE.andMap (field "url" D.string)
