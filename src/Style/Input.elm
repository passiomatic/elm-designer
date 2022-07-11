module Style.Input exposing
    ( LabelPosition(..)
    , labelPositionName
    , setLabelColor
    , setLabelPosition
    , setSliderMax
    , setSliderMin
    , setSliderSlider
    )

import Element exposing (Color)
import Style.Font exposing (Local)


{-| Label properties.
-}
type LabelPosition
    = LabelAbove
    | LabelBelow
    | LabelLeft
    | LabelRight
    | LabelHidden


labelPositionName position =
    case position of
        LabelAbove ->
            "Above"

        LabelBelow ->
            "Below"

        LabelLeft ->
            "Left"

        LabelRight ->
            "Right"

        LabelHidden ->
            "Hidden"


setLabelPosition : LabelPosition -> { a | position : LabelPosition } -> { a | position : LabelPosition }
setLabelPosition value record =
    { record | position = value }


setLabelColor : Local Color -> { a | color : Local Color } -> { a | color : Local Color }
setLabelColor value record =
    { record | color = value }


setSliderMin : Float -> { a | min : Float } -> { a | min : Float }
setSliderMin value record =
    { record | min = value }


setSliderMax : Float -> { a | max : Float } -> { a | max : Float }
setSliderMax value record =
    { record | max = value }


setSliderSlider : Maybe Float -> { a | step : Maybe Float } -> { a | step : Maybe Float }
setSliderSlider value record =
    { record | step = value }
