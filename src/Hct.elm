module Hct exposing (toRgb, fromRgb)

{-|

@docs toRgb, fromRgb

-}

import Basics.Extra exposing (fractionalModBy)
import Cam16 exposing (Cam16, defaultViewingConditions)
import List.Extra
import Utils exposing (Rgb, delinearized, lstarFromColor, matrixMultiply, rgbFromLinrgb, sanitizeAlpha, sanitizeDegrees, signum, yFromLstar)


{-| Convert HCT representation of a color in default viewing conditions to RGB.

  - **hue**: 0 <= hue < 360; invalid values are corrected.
  - **chroma**: 0 <= chroma < ?; Informally, colorfulness. The color returned
    may be lower than the requested chroma. Chroma has a different maximum for
    any given hue and tone.
  - **tone**: 0 <= tone <= 100; invalid values are corrected.
  - **alpha**: 0 <= alpha <= 1; invalid values are corrected.

-}
toRgb :
    { hue : Float
    , chroma : Float
    , tone : Float
    , alpha : Float
    }
    ->
        { red : Float
        , green : Float
        , blue : Float
        , alpha : Float
        }
toRgb { hue, chroma, tone, alpha } =
    let
        correctedAlpha : Float
        correctedAlpha =
            sanitizeAlpha alpha

        lstar : Float
        lstar =
            tone

        y : Float
        y =
            yFromLstar lstar
    in
    if chroma < 0.0001 || lstar < 0.0001 || lstar > 99.9999 then
        let
            component : Float
            component =
                delinearized y
        in
        { red = component
        , green = component
        , blue = component
        , alpha = correctedAlpha
        }

    else
        let
            hueRadians : Float
            hueRadians =
                hueDegrees / 180 * pi

            hueDegrees : Float
            hueDegrees =
                sanitizeDegrees hue
        in
        case findResultByJ hueRadians chroma y of
            Just exactAnswer ->
                { red = exactAnswer.red
                , green = exactAnswer.green
                , blue = exactAnswer.blue
                , alpha = correctedAlpha
                }

            Nothing ->
                let
                    rgb : Rgb
                    rgb =
                        bisectToLimit y hueRadians
                            |> rgbFromLinrgb
                in
                { red = rgb.red
                , green = rgb.green
                , blue = rgb.blue
                , alpha = correctedAlpha
                }


{-| Convert RGB representation of a color to HCT.
-}
fromRgb :
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }
    ->
        { hue : Float
        , chroma : Float
        , tone : Float
        , alpha : Float
        }
fromRgb color =
    let
        cam : Cam16
        cam =
            Cam16.fromRgb color defaultViewingConditions
    in
    { hue = cam.hue
    , chroma = cam.chroma
    , tone = lstarFromColor color
    , alpha = color.alpha
    }



--


scaledDiscountFromLinrgbMatrix :
    ( ( Float, Float, Float )
    , ( Float, Float, Float )
    , ( Float, Float, Float )
    )
scaledDiscountFromLinrgbMatrix =
    ( ( 0.001200833568784504
      , 0.002389694492170889
      , 0.0002795742885861124
      )
    , ( 0.0005891086651375999
      , 0.0029785502573438758
      , 0.0003270666104008398
      )
    , ( 0.00010146692491640572
      , 0.0005364214359186694
      , 0.0032979401770712076
      )
    )


yFromLinR : Float
yFromLinR =
    0.2126


yFromLinG : Float
yFromLinG =
    0.7152


yFromLinB : Float
yFromLinB =
    0.0722


criticalPlanes : List Float
criticalPlanes =
    [ 0.015176349177441876, 0.045529047532325624, 0.07588174588720938, 0.10623444424209313, 0.13658714259697685, 0.16693984095186062, 0.19729253930674434, 0.2276452376616281, 0.2579979360165119, 0.28835063437139563, 0.3188300904430532, 0.350925934958123, 0.3848314933096426, 0.42057480301049466, 0.458183274052838, 0.4976837250274023, 0.5391024159806381, 0.5824650784040898, 0.6277969426914107, 0.6751227633498623, 0.7244668422128921, 0.775853049866786, 0.829304845476233, 0.8848452951698498, 0.942497089126609, 1.0022825574869039, 1.0642236851973577, 1.1283421258858297, 1.1946592148522128, 1.2631959812511864, 1.3339731595349034, 1.407011200216447, 1.4823302800086415, 1.5599503113873272, 1.6398909516233677, 1.7221716113234105, 1.8068114625156377, 1.8938294463134073, 1.9832442801866852, 2.075074464868551, 2.1693382909216234, 2.2660538449872063, 2.36523901573795, 2.4669114995532007, 2.5710888059345764, 2.6777882626779785, 2.7870270208169257, 2.898822059350997, 3.0131901897720907, 3.1301480604002863, 3.2497121605402226, 3.3718988244681087, 3.4967242352587946, 3.624204428461639, 3.754355295633311, 3.887192587735158, 4.022731918402185, 4.160988767090289, 4.301978482107941, 4.445716283538092, 4.592217266055746, 4.741496401646282, 4.893568542229298, 5.048448422192488, 5.20615066083972, 5.3666897647573375, 5.5300801301023865, 5.696336044816294, 5.865471690767354, 6.037501145825082, 6.212438385869475, 6.390297286737924, 6.571091626112461, 6.7548350853498045, 6.941541251256611, 7.131223617812143, 7.323895587840543, 7.5195704746346665, 7.7182615035334345, 7.919981813454504, 8.124744458384042, 8.332562408825165, 8.543448553206703, 8.757415699253682, 8.974476575321063, 9.194643831691977, 9.417930041841839, 9.644347703669503, 9.873909240696694, 10.106627003236781, 10.342513269534024, 10.58158024687427, 10.8238400726681, 11.069304815507364, 11.317986476196008, 11.569896988756009, 11.825048221409341, 12.083451977536606, 12.345119996613247, 12.610063955123938, 12.878295467455942, 13.149826086772048, 13.42466730586372, 13.702830557985108, 13.984327217668513, 14.269168601521828, 14.55736596900856, 14.848930523210871, 15.143873411576273, 15.44220572664832, 15.743938506781891, 16.04908273684337, 16.35764934889634, 16.66964922287304, 16.985093187232053, 17.30399201960269, 17.62635644741625, 17.95219714852476, 18.281524751807332, 18.614349837764564, 18.95068293910138, 19.290534541298456, 19.633915083172692, 19.98083495742689, 20.331304511189067, 20.685334046541502, 21.042933821039977, 21.404114048223256, 21.76888489811322, 22.137256497705877, 22.50923893145328, 22.884842241736916, 23.264076429332462, 23.6469514538663, 24.033477234264016, 24.42366364919083, 24.817520537484558, 25.21505769858089, 25.61628489293138, 26.021211842414342, 26.429848230738664, 26.842203703840827, 27.258287870275353, 27.678110301598522, 28.10168053274597, 28.529008062403893, 28.96010235337422, 29.39497283293396, 29.83362889318845, 30.276079891419332, 30.722335150426627, 31.172403958865512, 31.62629557157785, 32.08401920991837, 32.54558406207592, 33.010999283389665, 33.4802739966603, 33.953417292456834, 34.430438229418264, 34.911345834551085, 35.39614910352207, 35.88485700094671, 36.37747846067349, 36.87402238606382, 37.37449765026789, 37.87891309649659, 38.38727753828926, 38.89959975977785, 39.41588851594697, 39.93615253289054, 40.460400508064545, 40.98864111053629, 41.520882981230194, 42.05713473317016, 42.597404951718396, 43.141702194811224, 43.6900349931913, 44.24241185063697, 44.798841244188324, 45.35933162437017, 45.92389141541209, 46.49252901546552, 47.065252796817916, 47.64207110610409, 48.22299226451468, 48.808024568002054, 49.3971762874833, 49.9904556690408, 50.587870934119984, 51.189430279724725, 51.79514187861014, 52.40501387947288, 53.0190544071392, 53.637271562750364, 54.259673423945976, 54.88626804504493, 55.517063457223934, 56.15206766869424, 56.79128866487574, 57.43473440856916, 58.08241284012621, 58.734331877617365, 59.39049941699807, 60.05092333227251, 60.715611475655585, 61.38457167773311, 62.057811747619894, 62.7353394731159, 63.417162620860914, 64.10328893648692, 64.79372614476921, 65.48848194977529, 66.18756403501224, 66.89098006357258, 67.59873767827808, 68.31084450182222, 69.02730813691093, 69.74813616640164, 70.47333615344107, 71.20291564160104, 71.93688215501312, 72.67524319850172, 73.41800625771542, 74.16517879925733, 74.9167682708136, 75.67278210128072, 76.43322770089146, 77.1981124613393, 77.96744375590167, 78.74122893956174, 79.51947534912904, 80.30219030335869, 81.08938110306934, 81.88105503125999, 82.67721935322541, 83.4778813166706, 84.28304815182372, 85.09272707154808, 85.90692527145302, 86.72564993000343, 87.54890820862819, 88.3767072518277, 89.2090541872801, 90.04595612594655, 90.88742016217518, 91.73345337380438, 92.58406282226491, 93.43925555268066, 94.29903859396902, 95.16341895893969, 96.03240364439274, 96.9059996312159, 97.78421388448044, 98.6670533535366, 99.55452497210776 ]


sanitizeRadians : Float -> Float
sanitizeRadians angle =
    fractionalModBy (pi * 2) (angle + pi * 8)


trueDelinearized : Float -> Float
trueDelinearized rgbComponent =
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
    delinearized_ * 255


chromaticAdaptation : Float -> Float
chromaticAdaptation component =
    let
        af : Float
        af =
            abs component ^ 0.42
    in
    signum component * 400 * af / (af + 27.13)


hueOf : Rgb -> Float
hueOf linrgb =
    let
        scaledDiscount : { r : Float, g : Float, b : Float }
        scaledDiscount =
            let
                ( r_, g_, b_ ) =
                    matrixMultiply
                        ( linrgb.red, linrgb.green, linrgb.blue )
                        scaledDiscountFromLinrgbMatrix
            in
            { r = r_, g = g_, b = b_ }

        rA : Float
        rA =
            chromaticAdaptation scaledDiscount.r

        gA : Float
        gA =
            chromaticAdaptation scaledDiscount.g

        bA : Float
        bA =
            chromaticAdaptation scaledDiscount.b

        -- redness-greenness
        a : Float
        a =
            (11 * rA + -12 * gA + bA) / 11

        -- yellowness-blueness
        b : Float
        b =
            (rA + gA - 2 * bA) / 9
    in
    atan2 b a


areInCyclicOrder : Float -> Float -> Float -> Bool
areInCyclicOrder a b c =
    let
        deltaAB : Float
        deltaAB =
            sanitizeRadians (b - a)

        deltaAC : Float
        deltaAC =
            sanitizeRadians (c - a)
    in
    deltaAB < deltaAC


intercept : Float -> Float -> Float -> Float
intercept source mid target =
    (mid - source) / (target - source)


lerpPoint : Rgb -> Float -> Rgb -> Rgb
lerpPoint source t target =
    { red = source.red + (target.red - source.red) * t
    , green = source.green + (target.green - source.green) * t
    , blue = source.blue + (target.blue - source.blue) * t
    }


setCoordinate : Rgb -> Float -> Rgb -> (Rgb -> Float) -> Rgb
setCoordinate source coordinate target axis =
    let
        t : Float
        t =
            intercept (axis source) coordinate (axis target)
    in
    lerpPoint source t target


isBounded : Float -> Bool
isBounded x =
    0 <= x && x <= 100


{-| Finds the segment containing the desired color.

Returns a list of two sets of linear RGB coordinates, each corresponding to an
endpoint of the segment containing the desired color.

  - **y**: The Y value of the color.
  - **targetHue**: The hue of the color.

-}
bisectToSegment : Float -> Float -> ( Rgb, Rgb )
bisectToSegment y targetHue =
    List.range 0 11
        |> List.foldl
            (\n state ->
                let
                    mid : Rgb
                    mid =
                        nthVertex y n
                in
                if mid.red < 0 then
                    state

                else
                    let
                        midHue : Float
                        midHue =
                            hueOf mid
                    in
                    if not state.initialized then
                        { state
                            | left = mid
                            , right = mid
                            , leftHue = midHue
                            , rightHue = midHue
                            , initialized = True
                        }

                    else if
                        state.uncut
                            || areInCyclicOrder state.leftHue midHue state.rightHue
                    then
                        if areInCyclicOrder state.leftHue targetHue midHue then
                            { state
                                | right = mid
                                , rightHue = midHue
                                , uncut = False
                            }

                        else
                            { state
                                | left = mid
                                , leftHue = midHue
                                , uncut = False
                            }

                    else
                        state
            )
            (let
                invalidVertex : Rgb
                invalidVertex =
                    { red = -1
                    , green = -1
                    , blue = -1
                    }
             in
             { left = invalidVertex
             , right = invalidVertex
             , leftHue = 0
             , rightHue = 0
             , initialized = False
             , uncut = True
             }
            )
        |> (\{ left, right } -> ( left, right ))


{-| Returns the nth possible vertex of the polygonal intersection of the y plane
and the RGB cube, in linear RGB coordinates, if it exists. If this possible
vertex lies outside of the cube, [-1.0, -1.0, -1.0] is returned.

  - **y**: The Y value of the plane.
  - **n**: The zero-based index of the point. 0 <= n <= 11.

-}
nthVertex : Float -> Int -> Rgb
nthVertex y n =
    let
        kR : Float
        kR =
            yFromLinR

        kG : Float
        kG =
            yFromLinG

        kB : Float
        kB =
            yFromLinB

        coordA : Float
        coordA =
            if modBy 4 n <= 1 then
                0

            else
                100

        coordB : Float
        coordB =
            if modBy 2 n == 0 then
                0

            else
                100

        invalidRgb : Rgb
        invalidRgb =
            { red = -1
            , green = -1
            , blue = -1
            }
    in
    if n < 4 then
        let
            g : Float
            g =
                coordA

            b : Float
            b =
                coordB

            r : Float
            r =
                (y - g * kG - b * kB) / kR
        in
        if isBounded r then
            { red = r
            , green = g
            , blue = b
            }

        else
            invalidRgb

    else if n < 8 then
        let
            b : Float
            b =
                coordA

            r : Float
            r =
                coordB

            g : Float
            g =
                (y - r * kR - b * kB) / kG
        in
        if isBounded g then
            { red = r
            , green = g
            , blue = b
            }

        else
            invalidRgb

    else
        let
            r : Float
            r =
                coordA

            g : Float
            g =
                coordB

            b : Float
            b =
                (y - r * kR - g * kG) / kB
        in
        if isBounded b then
            { red = r
            , green = g
            , blue = b
            }

        else
            invalidRgb


midpoint : ( Rgb, Rgb ) -> Rgb
midpoint ( a, b ) =
    { red = (a.red + b.red) / 2
    , green = (a.green + b.green) / 2
    , blue = (a.blue + b.blue) / 2
    }


criticalPlaneBelow : Float -> Int
criticalPlaneBelow x =
    floor (x - 0.5)


criticalPlaneAbove : Float -> Int
criticalPlaneAbove x =
    ceiling (x - 0.5)


bisectToLimit : Float -> Float -> Rgb
bisectToLimit y targetHue =
    let
        ( initLeft, initRight ) =
            bisectToSegment y targetHue

        initLeftHue : Float
        initLeftHue =
            hueOf initLeft

        loop :
            (Rgb -> Float)
            -> Float
            ->
                { left : Rgb
                , leftHue : Float
                , lPlane : Int
                , right : Rgb
                , rPlane : Int
                }
            -> ( Rgb, Float, Rgb )
        loop axis i { left, leftHue, lPlane, right, rPlane } =
            if i < 8 then
                if abs rPlane - lPlane <= 1 then
                    ( left, leftHue, right )

                else
                    let
                        midHue : Float
                        midHue =
                            hueOf mid

                        mid : Rgb
                        mid =
                            setCoordinate left midPlaneCoordinate right axis

                        midPlaneCoordinate : Float
                        midPlaneCoordinate =
                            List.Extra.getAt mPlane criticalPlanes
                                |> Maybe.withDefault 0

                        mPlane : Int
                        mPlane =
                            floor ((toFloat lPlane + toFloat rPlane) / 2)
                    in
                    if areInCyclicOrder leftHue targetHue midHue then
                        loop axis
                            (i + 1)
                            { left = left
                            , leftHue = leftHue
                            , lPlane = lPlane
                            , right = mid
                            , rPlane = mPlane
                            }

                    else
                        loop axis
                            (i + 1)
                            { left = mid
                            , leftHue = midHue
                            , lPlane = mPlane
                            , right = right
                            , rPlane = rPlane
                            }

            else
                ( left, leftHue, right )
    in
    [ .red, .green, .blue ]
        |> List.foldl
            (\axis ( left, leftHue, right ) ->
                if axis left /= axis right then
                    let
                        ( lPlane, rPlane ) =
                            if axis left < axis right then
                                ( axis left
                                    |> trueDelinearized
                                    |> criticalPlaneBelow
                                , axis right
                                    |> trueDelinearized
                                    |> criticalPlaneAbove
                                )

                            else
                                ( axis left
                                    |> trueDelinearized
                                    |> criticalPlaneAbove
                                , axis right
                                    |> trueDelinearized
                                    |> criticalPlaneBelow
                                )
                    in
                    loop axis
                        0
                        { left = left
                        , leftHue = leftHue
                        , lPlane = lPlane
                        , right = right
                        , rPlane = rPlane
                        }

                else
                    ( left, leftHue, right )
            )
            ( initLeft, initLeftHue, initRight )
        |> (\( left, _, right ) -> ( left, right ))
        |> midpoint


inverseChromaticAdaptation : Float -> Float
inverseChromaticAdaptation adapted =
    let
        adaptedAbs : Float
        adaptedAbs =
            abs adapted

        base : Float
        base =
            max 0 (27.13 * adaptedAbs / (400.0 - adaptedAbs))
    in
    signum adapted * (base ^ (1.0 / 0.42))


findResultByJ :
    Float
    -> Float
    -> Float
    -> Maybe Rgb
findResultByJ hueRadians chroma y =
    let
        viewingConditions : Cam16.ViewingConditions
        viewingConditions =
            defaultViewingConditions

        tInnerCoeff : Float
        tInnerCoeff =
            1
                / ((1.64 - (0.29 ^ viewingConditions.n)) ^ 0.73)

        eHue : Float
        eHue =
            0.25 * (cos (hueRadians + 2) + 3.8)

        p1 : Float
        p1 =
            eHue * (50000 / 13) * viewingConditions.nc * viewingConditions.ncb

        hSin : Float
        hSin =
            sin hueRadians

        hCos : Float
        hCos =
            cos hueRadians

        go :
            number
            -> Float
            -> Maybe Rgb
        go iterationRound j =
            let
                jNormalized : Float
                jNormalized =
                    j / 100

                alpha : Float
                alpha =
                    if chroma == 0 || j == 0 then
                        0

                    else
                        chroma / sqrt jNormalized

                t : Float
                t =
                    (alpha * tInnerCoeff) ^ (1 / 0.9)

                ac : Float
                ac =
                    viewingConditions.aw
                        * (jNormalized ^ (1.0 / viewingConditions.c / viewingConditions.z))

                p2 : Float
                p2 =
                    ac / viewingConditions.nbb

                gamma : Float
                gamma =
                    (23 * (p2 + 0.305) * t)
                        / (23 * p1 + 11 * t * hCos + 108 * t * hSin)

                a : Float
                a =
                    gamma * hCos

                b : Float
                b =
                    gamma * hSin

                rA : Float
                rA =
                    (460 * p2 + 451 * a + 288 * b) / 1403

                gA : Float
                gA =
                    (460 * p2 - 891 * a - 261 * b) / 1403

                bA : Float
                bA =
                    (460 * p2 - 220 * a - 6300 * b) / 1403

                rCScaled : Float
                rCScaled =
                    inverseChromaticAdaptation rA

                gCScaled : Float
                gCScaled =
                    inverseChromaticAdaptation gA

                bCScaled : Float
                bCScaled =
                    inverseChromaticAdaptation bA

                linR : Float
                linR =
                    (rCScaled * 1373.2198709594231)
                        + (gCScaled * -1100.4251190754821)
                        + (bCScaled * -7.278681089101213)

                linG : Float
                linG =
                    (rCScaled * -271.815969077903)
                        + (gCScaled * 559.6580465940733)
                        + (bCScaled * -32.46047482791194)

                linB : Float
                linB =
                    (rCScaled * 1.9622899599665666)
                        + (gCScaled * -57.173814538844006)
                        + (bCScaled * 308.7233197812385)

                fnj : Float
                fnj =
                    yFromLinR * linR + yFromLinG * linG + yFromLinB * linB
            in
            if linR < 0 || linG < 0 || linB < 0 || fnj <= 0 then
                Nothing

            else if iterationRound == 4 || (abs (fnj - y) < 0.002) then
                if linR > 100.01 || linG > 100.01 || linB > 100.01 then
                    Nothing

                else
                    { red = linR, green = linG, blue = linB }
                        |> rgbFromLinrgb
                        |> Just

            else
                -- Iterates with Newton method,
                -- Using 2 * fn(j) / j as the approximation of fn'(j)
                go (iterationRound + 1) (j - (fnj - y) * j / (2 * fnj))
    in
    go 0 (sqrt y * 11)
