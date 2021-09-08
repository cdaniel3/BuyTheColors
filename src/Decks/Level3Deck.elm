module Decks.Level3Deck exposing (..)

import Cards exposing (..)


level3Deck : List Card
level3Deck =
    let
        initLvl3 =
            initFrom "Level3Deck"
    in
    [ initLvl3 "00007_4_White_3"
    , initLvl3 "00037_5_White_3"
    , initLvl3 "00070_4_Blue_3"
    , initLvl3 "00700_4_Red_3"
    , initLvl3 "03063_4_Blue_3"
    , initLvl3 "03070_5_Blue_3"
    , initLvl3 "05333_3_Red_3"
    , initLvl3 "06330_4_Green_3"
    , initLvl3 "07000_4_Green_3"
    , initLvl3 "07300_5_Green_3"
    , initLvl3 "30036_4_White_3"
    , initLvl3 "30335_3_Blue_3"
    , initLvl3 "30700_5_Red_3"
    , initLvl3 "33053_3_Green_3"
    , initLvl3 "33530_3_Black_3"
    , initLvl3 "33600_4_Red_3"
    , initLvl3 "53303_3_White_3"
    , initLvl3 "60303_4_Black_3"
    , initLvl3 "70000_4_Black_3"
    , initLvl3 "70003_5_Black_3"
    ]
