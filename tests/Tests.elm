module Tests exposing (fromRgb, toRgb, tone)

import Expect exposing (FloatingPointTolerance(..))
import Hct
import Test exposing (Test)
import Utils


toRgb : Test
toRgb =
    Test.test "toRgb" <|
        \_ ->
            { hue = 0
            , chroma = 50
            , tone = 60
            , alpha = 1
            }
                |> Hct.toRgb
                |> Expect.equal
                    { red = 0.8453008140403622
                    , green = 0.4371058362556718
                    , blue = 0.5784927491918573
                    , alpha = 1
                    }


fromRgb : Test
fromRgb =
    Test.test "fromRgb" <|
        \_ ->
            { red = 27 / 255
            , green = 180 / 255
            , blue = 228 / 255
            , alpha = 1
            }
                |> Hct.fromRgb
                |> Expect.equal
                    { hue = 228.5459745439122
                    , chroma = 51.54626431258992
                    , tone = 68.37068783092431
                    , alpha = 1
                    }


tone : Test
tone =
    Test.describe "tone"
        [ Test.test "lstarFromColor" <|
            \_ ->
                { red = 27 / 255
                , green = 180 / 255
                , blue = 228 / 255
                , alpha = 1
                }
                    |> Utils.lstarFromColor
                    |> Expect.within (Absolute 0.0001) 68.37068783092431
        , Test.test "linearized" <|
            \_ ->
                Utils.linearized 0.5432
                    |> Expect.within (Absolute 0.0001) 25.622734573783845
        ]
