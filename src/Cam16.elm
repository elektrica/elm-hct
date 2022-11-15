module Cam16 exposing (Cam16, ViewingConditions, defaultViewingConditions, fromRgb)

import Utils exposing (linearized, signum, whitePointD65, yFromLstar)


type alias Cam16 =
    { hue : Float
    , chroma : Float
    }


type alias ViewingConditions =
    { n : Float
    , aw : Float
    , nbb : Float
    , ncb : Float
    , c : Float
    , nc : Float
    , rgbD :
        { r : Float
        , g : Float
        , b : Float
        }
    , fl : Float
    , z : Float
    }



--


fromRgb :
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }
    -> ViewingConditions
    -> Cam16
fromRgb { red, green, blue } viewingConditions =
    let
        redL : Float
        redL =
            linearized red

        greenL : Float
        greenL =
            linearized green

        blueL : Float
        blueL =
            linearized blue

        x : Float
        x =
            0.41233895 * redL + 0.35762064 * greenL + 0.18051042 * blueL

        y : Float
        y =
            0.2126 * redL + 0.7152 * greenL + 0.0722 * blueL

        z : Float
        z =
            0.01932141 * redL + 0.11916382 * greenL + 0.95034478 * blueL

        rC : Float
        rC =
            0.401288 * x + 0.650173 * y - 0.051461 * z

        gC : Float
        gC =
            -0.250268 * x + 1.204414 * y + 0.045854 * z

        bC : Float
        bC =
            -0.002079 * x + 0.048952 * y + 0.953127 * z

        rD : Float
        rD =
            viewingConditions.rgbD.r * rC

        gD : Float
        gD =
            viewingConditions.rgbD.g * gC

        bD : Float
        bD =
            viewingConditions.rgbD.b * bC

        rAF : Float
        rAF =
            ((viewingConditions.fl * abs rD) / 100) ^ 0.42

        gAF : Float
        gAF =
            ((viewingConditions.fl * abs gD) / 100) ^ 0.42

        bAF : Float
        bAF =
            ((viewingConditions.fl * abs bD) / 100) ^ 0.42

        rA : Float
        rA =
            (signum rD * 400 * rAF) / (rAF + 27.13)

        gA : Float
        gA =
            (signum gD * 400 * gAF) / (gAF + 27.13)

        bA : Float
        bA =
            (signum bD * 400 * bAF) / (bAF + 27.13)

        a : Float
        a =
            (11 * rA + -12 * gA + bA) / 11

        b : Float
        b =
            (rA + gA - 2 * bA) / 9

        u : Float
        u =
            (20.0 * rA + 20.0 * gA + 21.0 * bA) / 20.0

        p2 : Float
        p2 =
            (40.0 * rA + 20.0 * gA + bA) / 20.0

        atan2_ : Float
        atan2_ =
            atan2 b a

        atanDegrees : Float
        atanDegrees =
            (atan2_ * 180) / pi

        hue : Float
        hue =
            if atanDegrees < 0 then
                atanDegrees + 360.0

            else if atanDegrees >= 360 then
                atanDegrees - 360.0

            else
                atanDegrees

        ac : Float
        ac =
            p2 * viewingConditions.nbb

        j : Float
        j =
            100.0
                * ((ac / viewingConditions.aw)
                    ^ (viewingConditions.c * viewingConditions.z)
                  )

        huePrime : Float
        huePrime =
            if hue < 20.14 then
                hue + 360

            else
                hue

        eHue : Float
        eHue =
            0.25 * (cos ((huePrime * pi) / 180.0 + 2.0) + 3.8)

        p1 : Float
        p1 =
            (50000.0 / 13.0) * eHue * viewingConditions.nc * viewingConditions.ncb

        t : Float
        t =
            (p1 * sqrt (a * a + b * b)) / (u + 0.305)

        alpha : Float
        alpha =
            (t ^ 0.9) * ((1.64 - (0.29 ^ viewingConditions.n)) ^ 0.73)
    in
    { hue = hue
    , chroma = alpha * sqrt (j / 100)
    }



--


defaultViewingConditions : ViewingConditions
defaultViewingConditions =
    let
        whitePoint : Utils.Xyz
        whitePoint =
            whitePointD65

        adaptingLuminance : Float
        adaptingLuminance =
            ((200 / pi) * yFromLstar 50) / 100

        backgroundLstar : number
        backgroundLstar =
            50

        surround : number
        surround =
            2

        discountingIlluminant : Bool
        discountingIlluminant =
            False

        rW : Float
        rW =
            whitePoint.x * 0.401288 + whitePoint.y * 0.650173 + whitePoint.z * -0.051461

        gW : Float
        gW =
            whitePoint.x * -0.250268 + whitePoint.y * 1.204414 + whitePoint.z * 0.045854

        bW : Float
        bW =
            whitePoint.x * -0.002079 + whitePoint.y * 0.048952 + whitePoint.z * 0.953127

        f : Float
        f =
            0.8 + surround / 10.0

        d_ : Float
        d_ =
            if discountingIlluminant then
                1

            else
                f * (1 - (1 / 3.6) * exp ((-adaptingLuminance - 42) / 92))

        d : Float
        d =
            if d_ > 1 then
                1

            else if d_ < 0 then
                0

            else
                d_

        rgbD : { r : Float, g : Float, b : Float }
        rgbD =
            { r = d * (100 / rW) + 1 - d
            , g = d * (100 / gW) + 1 - d
            , b = d * (100 / bW) + 1 - d
            }

        k : Float
        k =
            1 / (5 * adaptingLuminance + 1)

        k4 : Float
        k4 =
            k ^ 4

        k4F : Float
        k4F =
            1 - k4

        fl : Float
        fl =
            k4 * adaptingLuminance + 0.1 * k4F * k4F * cubeRoot (5 * adaptingLuminance)

        n : Float
        n =
            yFromLstar backgroundLstar / whitePoint.y

        nbb : Float
        nbb =
            0.725 / (n ^ 0.2)

        rAFactor : Float
        rAFactor =
            ((fl * rgbD.r * rW) / 100) ^ 0.42

        gAFactor : Float
        gAFactor =
            ((fl * rgbD.g * gW) / 100) ^ 0.42

        bAFactor : Float
        bAFactor =
            ((fl * rgbD.b * bW) / 100) ^ 0.42

        rA : Float
        rA =
            (400 * rAFactor) / (rAFactor + 27.13)

        gA : Float
        gA =
            (400 * gAFactor) / (gAFactor + 27.13)

        bA : Float
        bA =
            (400 * bAFactor) / (bAFactor + 27.13)
    in
    { n = n
    , aw = (2 * rA + gA + 0.05 * bA) * nbb
    , nbb = nbb
    , ncb = nbb
    , c =
        if f >= 0.9 then
            lerp 0.59 0.69 ((f - 0.9) * 10.0)

        else
            lerp 0.525 0.59 ((f - 0.8) * 10.0)
    , nc = f
    , rgbD = rgbD
    , fl = fl
    , z = 1.48 + sqrt n
    }


exp : Float -> Float
exp n =
    eulersNumber ^ n


eulersNumber : Float
eulersNumber =
    2.718281828459045


cubeRoot : Float -> Float
cubeRoot n =
    n ^ (1 / 3)


lerp : Float -> Float -> Float -> Float
lerp start stop amount =
    (1 - amount) * start + amount * stop
