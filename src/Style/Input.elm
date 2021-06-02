module Style.Input exposing (LabelPosition(..), labelPositionName, setLabelPosition)

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
