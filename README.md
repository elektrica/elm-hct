# elm-hct

HCT, hue, chroma, and tone. A color system that provides a perceptually accurate
color measurement system. Built using
[CAM16](https://en.wikipedia.org/wiki/Color_appearance_model) hue and chroma,
and L* from L*a*b*.

Ported from
[Material color utilities](https://github.com/material-foundation/material-color-utilities).

## Example

```elm
import Hct

myHct :
    { hue : Float
    , chroma : Float
    , tone : Float
    , alpha : Float
    }
myHct =
    Hct.fromRgb
        { red = 0.8
        , green = 0.2
        , blue = 0.7
        , alpha = 1
        }


myRgb :
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }
myRgb =
    Hct.toRgb
        { hue = 270
        , chroma = 80
        , tone = 70
        , alpha = 1
        }
```
