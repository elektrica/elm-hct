module Utils exposing (Rgb, Xyz, delinearized, linearized, lstarFromColor, rgbFromLinrgb, sanitizeAlpha, sanitizeDegrees, signum, whitePointD65, yFromLstar)

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
    in
    { x = (r * 0.41233895) + (r * 0.2126) + (r * 0.01932141)
    , y = (g * 0.35762064) + (g * 0.7152) + (g * 0.11916382)
    , z = (b * 0.18051042) + (b * 0.0722) + (b * 0.95034478)
    }


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
    let
        sanitized : Float
        sanitized =
            fractionalModBy 1 alpha
    in
    if alpha < 0 then
        sanitized + 1

    else
        sanitized
