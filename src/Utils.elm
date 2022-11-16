module Utils exposing (Rgb, Xyz, delinearized, linearized, lstarFromColor, matrixMultiply, rgbFromLinrgb, sanitizeAlpha, sanitizeDegrees, signum, whitePointD65, yFromLstar)

import Basics.Extra exposing (fractionalModBy)


type alias Rgb =
    { red : Float
    , green : Float
    , blue : Float
    }


type alias Xyz =
    { x : Float
    , y : Float
    , z : Float
    }


srgbToXyzMatrix :
    ( ( Float, Float, Float )
    , ( Float, Float, Float )
    , ( Float, Float, Float )
    )
srgbToXyzMatrix =
    ( ( 0.41233895, 0.35762064, 0.18051042 )
    , ( 0.2126, 0.7152, 0.0722 )
    , ( 0.01932141, 0.11916382, 0.95034478 )
    )


rgbFromLinrgb : Rgb -> Rgb
rgbFromLinrgb { red, green, blue } =
    { red = delinearized red
    , green = delinearized green
    , blue = delinearized blue
    }


xyzFromColor :
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }
    -> Xyz
xyzFromColor { red, green, blue } =
    let
        r : Float
        r =
            linearized red

        g : Float
        g =
            linearized green

        b : Float
        b =
            linearized blue

        ( x, y, z ) =
            matrixMultiply ( r, g, b ) srgbToXyzMatrix
    in
    { x = x, y = y, z = z }


lstarFromColor :
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }
    -> Float
lstarFromColor color =
    let
        { y } =
            xyzFromColor color
    in
    116 * labF (y / 100) - 16


yFromLstar : Float -> Float
yFromLstar lstar =
    100 * labInvf ((lstar + 16) / 116)


linearized : Float -> Float
linearized rgbComponent =
    if rgbComponent <= 0.040449936 then
        rgbComponent / 12.92 * 100

    else
        (((rgbComponent + 0.055) / 1.055) ^ 2.4) * 100


delinearized : Float -> Float
delinearized rgbComponent =
    let
        normalized : Float
        normalized =
            rgbComponent / 100

        delinearized_ : Float
        delinearized_ =
            if normalized <= 0.0031308 then
                normalized * 12.92

            else
                1.055 * (normalized ^ (1 / 2.4)) - 0.055
    in
    clamp 0 1 delinearized_


whitePointD65 : Xyz
whitePointD65 =
    { x = 95.047
    , y = 100
    , z = 108.883
    }


labF : Float -> Float
labF t =
    let
        e : Float
        e =
            216 / 24389
    in
    if t > e then
        t ^ (1 / 3)

    else
        (kappa * t + 16) / 116


labInvf : Float -> Float
labInvf ft =
    let
        e : Float
        e =
            216 / 24389

        ft3 : Float
        ft3 =
            ft ^ 3
    in
    if ft3 > e then
        ft3

    else
        (116 * ft - 16) / kappa


kappa : Float
kappa =
    24389 / 27



-- MATH


signum : Float -> Float
signum num =
    if num < 0 then
        -1

    else if num == 0 then
        0

    else
        1


sanitizeDegrees : Float -> Float
sanitizeDegrees degrees =
    let
        sanitized : Float
        sanitized =
            fractionalModBy 360 degrees
    in
    if degrees < 0 then
        sanitized + 360

    else
        sanitized


sanitizeAlpha : Float -> Float
sanitizeAlpha alpha =
    alpha
        |> max 0
        |> min 1


matrixMultiply :
    ( number, number, number )
    ->
        ( ( number, number, number )
        , ( number, number, number )
        , ( number, number, number )
        )
    -> ( number, number, number )
matrixMultiply ( row0, row1, row2 ) ( matrix0, matrix1, matrix2 ) =
    let
        ( matrix00, matrix01, matrix02 ) =
            matrix0

        ( matrix10, matrix11, matrix12 ) =
            matrix1

        ( matrix20, matrix21, matrix22 ) =
            matrix2
    in
    ( row0 * matrix00 + row1 * matrix01 + row2 * matrix02
    , row0 * matrix10 + row1 * matrix11 + row2 * matrix12
    , row0 * matrix20 + row1 * matrix21 + row2 * matrix22
    )
