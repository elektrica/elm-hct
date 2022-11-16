module Tests exposing (..)

import Color
import Color.Convert
import Expect exposing (Expectation, FloatingPointTolerance(..))
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
                |> Color.fromRgba
                |> Color.Convert.colorToHex
                |> Expect.equal "#d86f94"


fromRgb : Test
fromRgb =
    Test.test "fromRgb" <|
        \_ ->
            "#1bb4e4"
                |> Color.Convert.hexToColor
                |> Result.map
                    (Color.toRgba
                        >> Hct.fromRgb
                        >> Expect.equal
                            { hue = 228.5459745439122
                            , chroma = 51.54626431258992
                            , tone = 68.37068783092431
                            , alpha = 1
                            }
                    )
                |> okExpectation


tone : Test
tone =
    Test.describe "tone"
        [ Test.test "lstarFromColor" <|
            \_ ->
                "#1bb4e4"
                    |> Color.Convert.hexToColor
                    |> Result.map
                        (Color.toRgba
                            >> Utils.lstarFromColor
                            >> Expect.within (Absolute 0.0001) 68.37068783092431
                        )
                    |> okExpectation
        , Test.test "linearized" <|
            \_ ->
                Utils.linearized 0.5432
                    |> Expect.within (Absolute 0.0001) 25.622734573783845
        ]


okExpectation : Result String Expectation -> Expectation
okExpectation result =
    case result of
        Ok expectation ->
            expectation

        Err error ->
            Expect.fail error
