module Views.Common exposing (fieldId, none)

import Html as H exposing (Html)
import Model exposing (Field(..))


fieldId : Field -> String
fieldId field =
    case field of
        FontSizeField ->
            "font-size"

        FontColorField ->
            "font-color-hex"

        BackgroundColorField ->
            "background-color-hex"

        PositionTopField ->
            "position-top"

        PositionRightField ->
            "position-right"

        PositionBottomField ->
            "position-bottom"

        PositionLeftField ->
            "position-left"

        PaddingTopField ->
            "padding-top"

        PaddingRightField ->
            "padding-right"

        PaddingBottomField ->
            "padding-bottom"

        PaddingLeftField ->
            "padding-left"

        SpacingXField ->
            "spacing-x"

        SpacingYField ->
            "spacing-y"

        ImageSrcField ->
            "image-src"

        BackgroundImageField ->
            "background-image"

        BorderColorField ->
            "border-color-hex"

        BorderTopLeftCornerField ->
            "border-top-left-corner"

        BorderTopRightCornerField ->
            "border-top-right-corner"

        BorderBottomRightCornerField ->
            "border-bottom-right-corner"

        BorderBottomLeftCornerField ->
            "border-bottom-left-corner"

        BorderTopWidthField ->
            "border-top-width"

        BorderRightWidthField ->
            "border-right-width"

        BorderBottomWidthField ->
            "border-bottom-width"

        BorderLeftWidthField ->
            "border-left-width"

        LabelField ->
            "label"


none =
    H.div [] []
