module Views.Common exposing
    ( addTooltipDown
    , addTooltipLeft
    , addTooltipRight
    , addTooltipUp
    , canDropInto
    , canDropNextTo
    , isDragging
    , none
    , widgetId
    )

import Document exposing (DragId(..))
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html5.DragDrop as DragDrop
import Model exposing (Msg, Widget(..))
import Tree as T exposing (Tree)


addTooltipUp =
    addTooltip "up"


addTooltipDown =
    addTooltip "down"


addTooltipLeft =
    addTooltip "left"


addTooltipRight =
    addTooltip "right"


addTooltip : String -> String -> List (Attribute Msg) -> List (Attribute Msg)
addTooltip position text attrs =
    if String.isEmpty text then
        -- Do not create empty tooltip
        attrs

    else
        A.attribute "aria-label" text
            --:: A.attribute "data-balloon-length" "medium"
            :: A.attribute "data-balloon-pos" position
            :: attrs


isDragging dragDrop =
    DragDrop.getDragId dragDrop /= Nothing


canDropInto container dragDrop =
    case DragDrop.getDragId dragDrop of
        Just dragId ->
            case dragId of
                Move node ->
                    Document.canInsertInto container node.type_

                Drag node ->
                    Document.canInsertInto container node.type_

                Insert template ->
                    Document.canInsertInto container (T.label template).type_

        Nothing ->
            False


canDropNextTo sibling dragDrop =
    case DragDrop.getDragId dragDrop of
        Just dragId ->
            case dragId of
                Move node ->
                    -- Do not allow to drop an element sibling to itself
                    (Document.nodeId sibling.id /= Document.nodeId node.id) && Document.canInsertNextTo sibling node.type_

                Drag node ->
                    Document.canInsertNextTo sibling node.type_

                Insert template ->
                    Document.canInsertNextTo sibling (T.label template).type_

        Nothing ->
            False


widgetId : Widget -> String
widgetId field =
    case field of
        FontSizeField ->
            "font-size"

        FontColorField ->
            "font-color-hex"

        LetterSpacingField ->
            "letter-spacing"

        WordSpacingField ->
            "word-spacing"

        BackgroundColorField ->
            "background-color-hex"

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

        LabelColorField ->
            "label-color"

        OffsetXField ->
            "offset-x"

        OffsetYField ->
            "offset-y"

        WidthMinField ->
            "width-min"

        WidthMaxField ->
            "width-max"

        WidthPxField ->
            "width-px"

        WidthPortionField ->
            "width-portion"

        HeightMinField ->
            "height-min"

        HeightMaxField ->
            "height-max"

        HeightPxField ->
            "height-px"

        HeightPortionField ->
            "height-portion"

        ShadowOffsetXField ->
            "shadow-offset-x"

        ShadowOffsetYField ->
            "shadow-offset-y"

        ShadowSizeField ->
            "shadow-size"

        ShadowColorField ->
            "shadow-color-hex"

        ShadowBlurField ->
            "shadow-blur"

        InsertDropdown ->
            "insert"


none =
    H.div [] []
