module Views.Inspector exposing (view)

{- Inspector allows to edit node style information. -}

import Codecs
import Css
import Dict exposing (Dict)
import Document exposing (..)
import Element exposing (Color, Orientation(..))
import Fonts
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Entity as Entity
import Html.Events as E
import Html.Keyed as Keyed
import Icons
import Model exposing (..)
import Palette
import Style.Background as Background exposing (Background)
import Style.Border as Border exposing (BorderCorner, BorderStyle(..), BorderWidth)
import Style.Font as Font exposing (..)
import Style.Input as Input exposing (..)
import Style.Layout as Layout exposing (..)
import Style.Shadow as Shadow exposing (Shadow)
import Style.Theme as Theme
import Tree as T exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import Views.Common as Common exposing (none, widgetId)


view : Model -> List (Html Msg)
view model =
    let
        zipper =
            model.document.present
    in
    [ H.form [ E.onSubmit FieldEditingConfirmed ]
        -- This makes onSubmit to work when hitting enter
        (H.button [ A.type_ "submit", A.class "visually-hidden" ] []
            :: resolveStyleViews model zipper
        )
    ]


resolveStyleViews : Model -> Zipper Node -> List (Html Msg)
resolveStyleViews model zipper =
    let
        node =
            Zipper.label zipper

        title =
            H.div [ A.class "bpx-3 bpt-3 fw-500" ]
                [ H.text (Document.nodeType node.type_) ]
    in
    title
        :: (case node.type_ of
                DocumentNode ->
                    []

                PageNode ->
                    [ sectionView "Layout"
                        [ pageLengthView model node
                        , spacingYView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        ]
                    , backgroundView model node
                    ]

                ImageNode image ->
                    [ sectionView "Info"
                        [ imageView image model node
                        ]
                    , sectionView "Layout"
                        [ alignmentView model node
                        , positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , bordersView model node
                    , backgroundView model node
                    , shadowView model node
                    ]

                RowNode data ->
                    [ sectionView ""
                        [ wrapRowOptionView data.wrapped
                        ]
                    , sectionView "Layout"
                        [ alignmentView model node
                        , positionView model node
                        , lengthView model node
                        , spacingXView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        ]
                    , bordersView model node
                    , backgroundView model node
                    , shadowView model node
                    ]

                ColumnNode ->
                    [ sectionView "Layout"
                        [ alignmentView model node
                        , positionView model node
                        , lengthView model node
                        , spacingYView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        ]
                    , bordersView model node
                    , backgroundView model node
                    , shadowView model node
                    ]

                TextColumnNode ->
                    [ sectionView "Layout"
                        [ alignmentView model node
                        , positionView model node
                        , lengthView model node
                        , spacingView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        ]
                    , bordersView model node
                    , backgroundView model node
                    , shadowView model node
                    ]

                TextFieldNode label ->
                    [ sectionView "Label"
                        [ labelTextView label model node
                        , labelPositionView label model node
                        , labelColorView label model zipper
                        , spacingYView model node
                        ]
                    , sectionView "Layout"
                        [ alignmentView model node
                        , positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        , fontSpacingView model node
                        , textAlignmentView model node.textAlignment
                        ]
                    , bordersView model node
                    , backgroundView model node
                    , shadowView model node
                    ]

                TextFieldMultilineNode label ->
                    [ sectionView "Label"
                        [ labelTextView label model node
                        , labelPositionView label model node
                        , labelColorView label model zipper
                        , spacingYView model node
                        ]
                    , sectionView "Layout"
                        [ alignmentView model node
                        , positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        , fontSpacingView model node
                        , textAlignmentView model node.textAlignment
                        ]
                    , bordersView model node
                    , backgroundView model node
                    , shadowView model node
                    ]

                CheckboxNode label ->
                    [ sectionView "Label"
                        [ labelTextView label model node
                        , labelPositionView label model node
                        , spacingXView model node
                        ]
                    , sectionView "Layout"
                        [ alignmentView model node
                        , positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        , fontSpacingView model node
                        , textAlignmentView model node.textAlignment
                        ]
                    , bordersView model node
                    , backgroundView model node
                    , shadowView model node
                    ]

                ButtonNode button ->
                    [ sectionView "Label"
                        [ labelTextView button model node
                        ]
                    , sectionView "Layout"
                        [ alignmentView model node
                        , positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        , fontSpacingView model node
                        , textAlignmentView model node.textAlignment
                        ]
                    , bordersView model node
                    , backgroundView model node
                    , shadowView model node
                    ]

                RadioNode label ->
                    [ sectionView "Label"
                        [ labelTextView label model node
                        , labelPositionView label model node
                        ]
                    , sectionView "Layout"
                        [ alignmentView model node
                        , positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        ]
                    , bordersView model node
                    , backgroundView model node
                    , shadowView model node
                    ]

                OptionNode option ->
                    [ sectionView "Label"
                        [ labelTextView option model node
                        ]
                    , sectionView "Layout"
                        [ alignmentView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        , fontSpacingView model node
                        ]
                    , bordersView model node
                    , backgroundView model node
                    , shadowView model node
                    ]

                _ ->
                    commonViews zipper model node
           )


commonViews zipper model node =
    [ sectionView "Layout"
        [ alignmentView model node
        , positionView model node
        , lengthView model node
        , paddingView model node
        ]
    , sectionView "Text"
        [ fontView model zipper
        , fontSpacingView model node
        , textAlignmentView model node.textAlignment
        ]
    , bordersView model node
    , backgroundView model node
    , shadowView model node
    ]


sectionView : String -> List (Html Msg) -> Html Msg
sectionView title views =
    H.section [ A.class "section bp-3 border-bottom" ]
        ((if String.isEmpty title then
            none

          else
            H.h2 [ A.class "section__title mb-2" ]
                [ H.text title ]
         )
            :: views
        )


labelTextView : { a | text : String } -> Model -> Node -> Html Msg
labelTextView { text } model { type_ } =
    let
        label_ =
            case model.inspector of
                EditingField LabelField new ->
                    new

                _ ->
                    text
    in
    H.div [ A.class "row align-items-center mb-2" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Text" ]
        , H.div [ A.class "col-9" ]
            [ H.input
                [ A.id (widgetId LabelField)
                , A.type_ "text"
                , A.value label_
                , A.placeholder ""
                , A.class "form-control form-control-sm"
                , E.onFocus (FieldEditingStarted LabelField label_)
                , E.onBlur FieldEditingFinished
                , E.onInput FieldChanged
                ]
                []
            ]
        ]


labelColorView : { a | color : Local Color } -> Model -> Zipper Node -> Html Msg
labelColorView { color } model zipper =
    let
        theme =
            Theme.defaultTheme

        resolvedFontColor =
            case color of
                Local value ->
                    value

                Inherited ->
                    Document.resolveInheritedFontColor theme.textColor zipper
    in
    colorView model (Just resolvedFontColor) LabelColorField LabelColorChanged


labelPositionView : { a | position : LabelPosition } -> Model -> Node -> Html Msg
labelPositionView { position } model { type_ } =
    let
        setSelected other attrs =
            A.selected (position == other) :: attrs
    in
    H.div [ A.class "row align-items-center mb-2" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Position" ]
        , H.div [ A.class "col-9" ]
            [ Keyed.node "select"
                [ onLabelPositionSelect LabelPositionChanged, A.class "form-select form-select-sm" ]
                (List.map
                    (\position_ ->
                        let
                            name =
                                Input.labelPositionName position_
                        in
                        ( name
                        , H.option (setSelected position_ [ labelPositionValue position_ ])
                            [ H.text name ]
                        )
                    )
                    labelPositions
                )
            ]
        ]


labelPositions =
    [ LabelAbove, LabelBelow, LabelLeft, LabelRight, LabelHidden ]


labelPositionValue : LabelPosition -> Attribute msg
labelPositionValue value =
    A.value (Codecs.encodeLabelPosition value)


onLabelPositionSelect msg =
    E.on "input" (Codecs.labelPositionDecoder msg)



-- imageView : ImageData -> Model -> Node -> Html Msg
-- imageView image model _ =
--     let
--         imageSrc =
--             case model.inspector of
--                 EditingField ImageSrcField new ->
--                     new
--                 _ ->
--                     image.src
--     in
--     H.section [ A.class "section bp-3  border-bottom" ]
--         [ H.h2 [ A.class "section__title mb-2" ]
--             [ H.text "Image" ]
--         , H.div [ A.class "" ]
--             [ H.div [ A.class "" ]
--                 [ H.input
--                     [ A.id (widgetId ImageSrcField)
--                     , A.type_ "text"
--                     , A.value imageSrc
--                     , A.placeholder "https://domain.com/sample.jpg"
--                     , A.class "form-control form-control-sm"
--                     , E.onFocus (FieldEditingStarted ImageSrcField imageSrc)
--                     , E.onBlur FieldEditingFinished
--                     , E.onInput FieldChanged
--                     ]
--                     []
--                 , H.label [ A.for (widgetId ImageSrcField), A.class "small m-0" ]
--                     [ H.text "Image address" ]
--                 ]
--             ]
--         ]


paddingView : Model -> Node -> Html Msg
paddingView model { padding } =
    let
        paddingTop =
            case model.inspector of
                EditingField PaddingTopField new ->
                    new

                EditingField PaddingRightField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.top

                EditingField PaddingBottomField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.top

                EditingField PaddingLeftField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.top

                _ ->
                    String.fromInt padding.top

        paddingRight =
            case model.inspector of
                EditingField PaddingRightField new ->
                    new

                EditingField PaddingTopField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.right

                EditingField PaddingBottomField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.right

                EditingField PaddingLeftField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.right

                _ ->
                    String.fromInt padding.right

        paddingBottom =
            case model.inspector of
                EditingField PaddingBottomField new ->
                    new

                EditingField PaddingTopField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.bottom

                EditingField PaddingRightField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.bottom

                EditingField PaddingLeftField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.bottom

                _ ->
                    String.fromInt padding.bottom

        paddingLeft =
            case model.inspector of
                EditingField PaddingLeftField new ->
                    new

                EditingField PaddingTopField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.left

                EditingField PaddingRightField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.left

                EditingField PaddingBottomField new ->
                    if padding.locked then
                        new

                    else
                        String.fromInt padding.left

                _ ->
                    String.fromInt padding.left
    in
    H.div [ A.class "row align-items-center" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Padding" ]
        , H.div [ A.class "col-9" ]
            [ H.div [ A.class "d-flex justify-content-center mb-1" ]
                [ H.div [ A.class "w-25" ]
                    [ H.input
                        [ A.id (widgetId PaddingTopField)
                        , A.type_ "number"
                        , A.min "0"
                        , A.value paddingTop
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted PaddingTopField paddingTop)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                ]
            , H.div [ A.class "d-flex align-items-center mb-1" ]
                [ H.div [ A.class "w-25" ]
                    [ H.input
                        [ A.id (widgetId PaddingLeftField)
                        , A.type_ "number"
                        , A.min "0"
                        , A.value paddingLeft
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted PaddingLeftField paddingLeft)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                , H.div
                    [ A.class "w-50 text-center"
                    ]
                    [ H.button
                        ([ A.classList
                            [ ( "btn btn-link", True )
                            , ( "text-dark", not padding.locked )
                            ]
                         , E.onClick (PaddingLockChanged (not padding.locked))
                         , A.type_ "button"
                         ]
                            |> Common.addTooltipDown "Use a single padding for all directions"
                        )
                        [ if padding.locked then
                            Icons.lock

                          else
                            Icons.unlock
                        ]
                    ]
                , H.div [ A.class "w-25" ]
                    [ H.input
                        [ A.id (widgetId PaddingRightField)
                        , A.type_ "number"
                        , A.min "0"
                        , A.value paddingRight
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted PaddingRightField paddingRight)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                ]
            , H.div [ A.class "d-flex justify-content-center" ]
                [ H.div [ A.class "w-25" ]
                    [ H.input
                        [ A.id (widgetId PaddingBottomField)
                        , A.type_ "number"
                        , A.min "0"
                        , A.value paddingBottom
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted PaddingBottomField paddingBottom)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                ]
            ]
        ]


{-| Horizontal and vertical spacing.
-}
spacingView : Model -> Node -> Html Msg
spacingView model { spacing } =
    let
        x =
            case spacing of
                Spacing ( x_, _ ) ->
                    case model.inspector of
                        EditingField SpacingXField new ->
                            new

                        _ ->
                            String.fromInt x_

                SpaceEvenly ->
                    "Evenly"

        y =
            case spacing of
                Spacing ( _, y_ ) ->
                    case model.inspector of
                        EditingField SpacingYField new ->
                            new

                        _ ->
                            String.fromInt y_

                SpaceEvenly ->
                    "Evenly"
    in
    H.div [ A.class "row align-items-center mb-3" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Spacing" ]
        , H.div [ A.class "col-9" ]
            [ H.div [ A.class "d-flex justify-content-end", A.style "gap" ".25rem" ]
                [ numericFieldView SpacingXField "X" x
                , numericFieldView SpacingYField "Y" y
                ]
            ]
        ]


{-| Horizontal spacing.
-}
spacingXView : Model -> Node -> Html Msg
spacingXView model { spacing } =
    let
        x =
            case spacing of
                Spacing ( x_, _ ) ->
                    case model.inspector of
                        EditingField SpacingXField new ->
                            new

                        _ ->
                            String.fromInt x_

                SpaceEvenly ->
                    "Evenly"
    in
    H.div [ A.class "row align-items-center mb-3" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Spacing" ]
        , H.div [ A.class "col-9" ]
            [ H.div [ A.class "d-flex justify-content-end" ]
                [ numericFieldView SpacingXField "X" x ]
            ]
        ]


{-| Vertical spacing.
-}
spacingYView : Model -> Node -> Html Msg
spacingYView model { spacing } =
    let
        y =
            case spacing of
                Spacing ( _, y_ ) ->
                    case model.inspector of
                        EditingField SpacingYField new ->
                            new

                        _ ->
                            String.fromInt y_

                SpaceEvenly ->
                    "Evenly"
    in
    H.div [ A.class "row align-items-center mb-3" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Spacing" ]
        , H.div [ A.class "col-9" ]
            [ H.div [ A.class "d-flex justify-content-end" ]
                [ numericFieldView SpacingYField "Y" y ]
            ]
        ]


addDropdown widgetId_ state items parent =
    let
        visible =
            case state of
                Visible id ->
                    id == widgetId_

                Hidden ->
                    False
    in
    H.div [ A.class "input-group" ]
        [ parent
        , H.div
            [ A.classList
                [ ( "dropdown-menu", True )
                , ( widgetId widgetId_ ++ "-dropdown", True )
                , ( "show", visible )
                ]
            ]
            items
        , H.button
            [ A.attribute "aria-haspopup" "true"
            , A.class "btn btn-outline-secondary btn-sm dropdown-toggle"
            , E.onClick
                (DropDownChanged
                    (if visible then
                        Hidden

                     else
                        Visible widgetId_
                    )
                )

            --, A.attribute "data-toggle" "dropdown"
            , A.type_ "button"
            ]
            [ H.span [ A.class "visually-hidden" ] [ H.text "Pick a font size" ] ]
        ]


shadowView : Model -> Node -> Html Msg
shadowView model { shadow } =
    let
        expanded =
            Shadow.hasSize shadow
    in
    H.section [ A.class "section bp-3  border-bottom" ]
        (H.div [ A.class "d-flex justify-content-between align-items-center mb-2" ]
            [ H.h2 [ A.class "section__title m-0" ] [ H.text "Shadow" ]
            , if expanded then
                removeStyleButtonView (SetShadowClicked Shadow.none)

              else
                addStyleButtonView (SetShadowClicked Shadow.default)
            ]
            :: (if expanded then
                    [ shadowOffsetSizeBlurView model shadow
                    , shadowTypeView model shadow
                    , colorView model (Just shadow.color) ShadowColorField ShadowColorChanged
                    ]

                else
                    []
               )
        )


shadowOffsetSizeBlurView model shadow =
    let
        offsetX =
            case model.inspector of
                EditingField ShadowOffsetXField new ->
                    new

                _ ->
                    String.fromFloat shadow.offsetX

        offsetY =
            case model.inspector of
                EditingField ShadowOffsetYField new ->
                    new

                _ ->
                    String.fromFloat shadow.offsetY

        size =
            case model.inspector of
                EditingField ShadowSizeField new ->
                    new

                _ ->
                    String.fromFloat shadow.size

        blur =
            case model.inspector of
                EditingField ShadowBlurField new ->
                    new

                _ ->
                    String.fromFloat shadow.blur
    in
    H.div [ A.class "row align-items-center mb-2" ]
        [ H.div [ A.class "col-12" ]
            [ H.div [ A.class "d-flex" ]
                [ H.div [ A.class "w-25 ms-auto me-1" ]
                    [ H.label [ A.class "col-form-label-sm m-0 p-0", A.for (widgetId ShadowOffsetXField) ]
                        [ H.text "Offset X"
                        ]
                    , H.input
                        [ A.id (widgetId ShadowOffsetXField)
                        , A.type_ "number"
                        , A.value offsetX
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted ShadowOffsetXField offsetX)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                , H.div [ A.class "w-25 me-1" ]
                    [ H.label [ A.class "col-form-label-sm m-0 p-0", A.for (widgetId ShadowOffsetXField) ]
                        [ H.text "Offset Y"
                        ]
                    , H.input
                        [ A.id (widgetId ShadowOffsetYField)
                        , A.type_ "number"
                        , A.value offsetY
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted ShadowOffsetYField offsetY)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                , H.div [ A.class "w-25 me-1" ]
                    [ H.label [ A.class "col-form-label-sm m-0 p-0", A.for (widgetId ShadowOffsetXField) ]
                        [ H.text "Size"
                        ]
                    , H.input
                        [ A.id (widgetId ShadowSizeField)
                        , A.type_ "number"
                        , A.min "0"
                        , A.value size
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted ShadowSizeField size)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                , H.div [ A.class "w-25" ]
                    [ H.label [ A.class "col-form-label-sm m-0 p-0", A.for (widgetId ShadowOffsetXField) ]
                        [ H.text "Blur"
                        ]
                    , H.input
                        [ A.id (widgetId ShadowBlurField)
                        , A.type_ "number"
                        , A.min "0"
                        , A.value blur
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted ShadowBlurField blur)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                ]
            ]
        ]


shadowTypeView : Model -> Shadow -> Html Msg
shadowTypeView model shadow =
    H.div [ A.class "row align-items-center mb-2" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Type" ]
        , H.div [ A.class "col-9 d-flex" ]
            [ H.div [ A.class "btn-group w-100", A.attribute "role" "group" ]
                [ H.button
                    [ A.classList
                        [ ( "btn btn-secondary btn-sm", True )
                        , ( "active", Shadow.isInner shadow.type_ )
                        ]
                    , E.onClick (ShadowTypeChanged Shadow.Inner)
                    , A.type_ "button"
                    ]
                    [ H.text "Inner" ]
                , H.button
                    [ A.classList
                        [ ( "btn btn-secondary btn-sm", True )
                        , ( "active", Shadow.isOuter shadow.type_ )
                        ]
                    , E.onClick (ShadowTypeChanged Shadow.Outer)
                    , A.type_ "button"
                    ]
                    [ H.text "Outer" ]
                ]
            ]
        ]


bordersView : Model -> Node -> Html Msg
bordersView model { borderColor, borderWidth, borderStyle, borderCorner } =
    let
        expanded =
            Border.hasWidth borderWidth || Border.isRounded borderCorner
    in
    H.section [ A.class "section bp-3  border-bottom" ]
        (H.div [ A.class "d-flex justify-content-between align-items-center mb-2" ]
            [ H.h2 [ A.class "section__title m-0" ] [ H.text "Border" ]
            , if expanded then
                removeStyleButtonView (SetBorderClicked (Border.width 0))

              else
                addStyleButtonView (SetBorderClicked (Border.width 1))
            ]
            :: (if expanded then
                    [ bordersWidthCornerView model borderWidth borderCorner
                    , borderStyleView model borderStyle
                    , colorView model (Just borderColor) BorderColorField BorderColorChanged
                    ]

                else
                    []
               )
        )


bordersWidthCornerView : Model -> BorderWidth -> BorderCorner -> Html Msg
bordersWidthCornerView model borderWidth borderCorner =
    let
        -- Corners
        topLeftCorner =
            case model.inspector of
                EditingField BorderTopLeftCornerField new ->
                    new

                _ ->
                    String.fromInt borderCorner.topLeft

        topRightCorner =
            case model.inspector of
                EditingField BorderTopRightCornerField new ->
                    new

                _ ->
                    String.fromInt borderCorner.topRight

        bottomRightCorner =
            case model.inspector of
                EditingField BorderBottomRightCornerField new ->
                    new

                _ ->
                    String.fromInt borderCorner.bottomRight

        bottomLeftCorner =
            case model.inspector of
                EditingField BorderBottomLeftCornerField new ->
                    new

                _ ->
                    String.fromInt borderCorner.bottomLeft

        -- Widths
        topWidth =
            case model.inspector of
                EditingField BorderTopWidthField new ->
                    new

                _ ->
                    String.fromInt borderWidth.top

        rightWidth =
            case model.inspector of
                EditingField BorderRightWidthField new ->
                    new

                _ ->
                    String.fromInt borderWidth.right

        bottomWidth =
            case model.inspector of
                EditingField BorderBottomWidthField new ->
                    new

                _ ->
                    String.fromInt borderWidth.bottom

        leftWidth =
            case model.inspector of
                EditingField BorderLeftWidthField new ->
                    new

                _ ->
                    String.fromInt borderWidth.left
    in
    H.div [ A.class "row align-items-center mb-2" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Size" ]
        , H.div [ A.class "col-9" ]
            [ H.div [ A.class "d-flex justify-content-between mb-1" ]
                [ H.div [ A.class "w-25 me-1" ]
                    [ H.div [ A.class "input-group input-group-sm" ]
                        [ H.span [ A.class "input-group-text bpx-1" ] [ H.text Entity.ulcorner ]
                        , H.input
                            [ A.id (widgetId BorderTopLeftCornerField)
                            , A.type_ "number"
                            , A.min "0"
                            , A.value topLeftCorner
                            , A.class "form-control form-control-sm text-center"
                            , E.onFocus (FieldEditingStarted BorderTopLeftCornerField topLeftCorner)
                            , E.onBlur FieldEditingFinished
                            , E.onInput FieldChanged
                            ]
                            []
                        ]
                    ]
                , H.div [ A.class "w-25 me-1" ]
                    [ H.input
                        [ A.id (widgetId BorderTopWidthField)
                        , A.type_ "number"
                        , A.min "0"
                        , A.value topWidth
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted BorderTopWidthField topWidth)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                , H.div [ A.class "w-25" ]
                    [ H.div [ A.class "input-group input-group-sm" ]
                        [ H.input
                            [ A.id (widgetId BorderTopRightCornerField)
                            , A.type_ "number"
                            , A.min "0"
                            , A.value topRightCorner
                            , A.class "form-control form-control-sm text-center"
                            , E.onFocus (FieldEditingStarted BorderTopRightCornerField topRightCorner)
                            , E.onBlur FieldEditingFinished
                            , E.onInput FieldChanged
                            ]
                            []
                        , H.span [ A.class "input-group-text bpx-1" ] [ H.text Entity.urcorner ]
                        ]
                    ]
                ]
            , H.div [ A.class "d-flex justify-content-between align-items-center mb-1" ]
                [ H.div [ A.class "w-25" ]
                    [ H.input
                        [ A.id (widgetId BorderLeftWidthField)
                        , A.type_ "number"
                        , A.min "0"
                        , A.value leftWidth
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted BorderLeftWidthField leftWidth)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                , H.div [ A.class "w-50 text-center" ]
                    [ H.button
                        ([ A.classList
                            [ ( "btn btn-link", True )
                            , ( "text-dark", not borderWidth.locked )
                            ]
                         , E.onClick (BorderLockChanged (not borderWidth.locked))
                         , A.type_ "button"
                         ]
                            |> Common.addTooltipDown "Use a single border for all directions"
                        )
                        [ if borderWidth.locked then
                            Icons.lock

                          else
                            Icons.unlock
                        ]
                    ]
                , H.div [ A.class "w-25" ]
                    [ H.input
                        [ A.id (widgetId BorderRightWidthField)
                        , A.type_ "number"
                        , A.min "0"
                        , A.value rightWidth

                        --, A.placeholder ""
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted BorderRightWidthField rightWidth)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                ]
            , H.div [ A.class "d-flex justify-content-between" ]
                [ H.div [ A.class "w-25 me-1" ]
                    [ H.div [ A.class "input-group input-group-sm" ]
                        [ H.span [ A.class "input-group-text bpx-1" ] [ H.text Entity.llcorner ]
                        , H.input
                            [ A.id (widgetId BorderBottomLeftCornerField)
                            , A.type_ "number"
                            , A.min "0"
                            , A.value bottomLeftCorner

                            --, A.placeholder ""
                            , A.class "form-control form-control-sm text-center"
                            , E.onFocus (FieldEditingStarted BorderBottomLeftCornerField bottomLeftCorner)
                            , E.onBlur FieldEditingFinished
                            , E.onInput FieldChanged
                            ]
                            []
                        ]
                    ]
                , H.div [ A.class "w-25 me-1" ]
                    [ H.input
                        [ A.id (widgetId BorderBottomWidthField)
                        , A.type_ "number"
                        , A.min "0"
                        , A.value bottomWidth

                        --, A.placeholder ""
                        , A.class "form-control form-control-sm text-center"
                        , E.onFocus (FieldEditingStarted BorderBottomWidthField bottomWidth)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                , H.div [ A.class "w-25" ]
                    [ H.div [ A.class "input-group input-group-sm" ]
                        [ H.input
                            [ A.id (widgetId BorderBottomRightCornerField)
                            , A.type_ "number"
                            , A.min "0"
                            , A.value bottomRightCorner

                            --, A.placeholder ""
                            , A.class "form-control form-control-sm text-center"
                            , E.onFocus (FieldEditingStarted BorderBottomRightCornerField bottomRightCorner)
                            , E.onBlur FieldEditingFinished
                            , E.onInput FieldChanged
                            ]
                            []
                        , H.span [ A.class "input-group-text bpx-1" ] [ H.text Entity.lrcorner ]
                        ]
                    ]
                ]
            ]
        ]


borderStyleView : Model -> BorderStyle -> Html Msg
borderStyleView model borderStyle =
    H.div [ A.class "row align-items-center mb-2" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Style" ]
        , H.div [ A.class "col-9 d-flex" ]
            [ H.div [ A.class "btn-group w-100", A.attribute "role" "group" ]
                [ H.button
                    [ A.classList
                        [ ( "btn btn-secondary btn-sm", True )
                        , ( "active", Border.isSolid borderStyle )
                        ]
                    , E.onClick (BorderStyleChanged Solid)
                    , A.type_ "button"
                    ]
                    [ H.text "Solid" ]
                , H.button
                    [ A.classList
                        [ ( "btn btn-secondary btn-sm", True )
                        , ( "active", Border.isDashed borderStyle )
                        ]
                    , E.onClick (BorderStyleChanged Dashed)
                    , A.type_ "button"
                    ]
                    [ H.text "Dashed" ]
                , H.button
                    [ A.classList
                        [ ( "btn btn-secondary btn-sm", True )
                        , ( "active", Border.isDotted borderStyle )
                        ]
                    , E.onClick (BorderStyleChanged Dotted)
                    , A.type_ "button"
                    ]
                    [ H.text "Dotted" ]
                ]
            ]
        ]


colorView : Model -> Maybe Color -> Widget -> (String -> Msg) -> Html Msg
colorView model color field msg =
    H.div [ A.class "row align-items-center mb-2" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Color" ]
        , H.div [ A.class "col-9 d-flex" ]
            [ colorPickerView model color msg
            , colorHexView model color field
            ]
        ]


colorPickerView : Model -> Maybe Color -> (String -> Msg) -> Html Msg
colorPickerView _ value msg =
    let
        value_ =
            Maybe.withDefault Palette.transparent value
    in
    H.input
        [ A.type_ "color"
        , A.value (Css.colorToStringWithHash value_)
        , E.onInput msg
        , A.classList
            [ ( "form-control form-control-sm me-1", True )
            , ( "transparent", value == Nothing )
            ]
        ]
        []


colorHexView : Model -> Maybe Color -> Widget -> Html Msg
colorHexView model color field =
    let
        color_ =
            case model.inspector of
                EditingField field_ new ->
                    if field == field_ then
                        new

                    else
                        Maybe.map Css.colorToString color
                            |> Maybe.withDefault ""

                _ ->
                    Maybe.map Css.colorToString color
                        |> Maybe.withDefault ""
    in
    H.div [ A.class "input-group input-group-sm" ]
        [ H.span [ A.class "input-group-text bpx-1" ] [ H.text "#" ]
        , H.input
            [ A.id (widgetId field)
            , A.type_ "text"
            , A.value color_
            , E.onFocus (FieldEditingStarted field color_)
            , E.onBlur FieldEditingFinished
            , E.onInput FieldChanged
            , A.class "form-control"
            ]
            []
        ]



-- colorAlphaView : Model -> String -> Color -> Html Msg
-- colorAlphaView _ name color =
--     H.div [ A.class "w-33" ]
--         [ H.label [ A.for (widgetId BorderColorField), A.class "small m-0" ]
--             [ H.text "Opacity" ]
--         , H.div [ A.class "input-group input-group-sm" ]
--             [ H.input
--                 [ A.id name
--                 , A.type_ "text"
--                 , A.value "100"
--                 , A.class "form-control form-control-sm"
--                 --, A.title "Color opacity"
--                 ]
--                 []
--               , H.span [ A.class "input-group-text bpx-1" ] [ H.text "%" ]
--             ]
--         -- , H.label [ A.for name, A.class "small m-0" ]
--         --     [ H.text "Opacity" ]
--         ]


backgroundView : Model -> Node -> Html Msg
backgroundView model { background } =
    H.section [ A.class "section bp-3 border-bottom" ]
        [ H.div [ A.class "d-flex justify-content-between align-items-center mb-2" ]
            [ H.h2 [ A.class "section__title m-0" ]
                [ H.text "Background" ]
            , if Background.isNone background then
                addStyleButtonView (BackgroundChanged (Background.solid Palette.lightGray))

              else
                removeStyleButtonView (BackgroundChanged Background.None)
            ]
        , case background of
            Background.Image value ->
                let
                    value_ =
                        case model.inspector of
                            EditingField BackgroundImageField new ->
                                new

                            _ ->
                                value
                in
                H.div []
                    [ backgroundToggleView model background
                    , H.div [ A.class "row align-items-center mb-2" ]
                        [ H.label [ A.for (widgetId BackgroundImageField), A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
                            [ H.text "Image URL" ]
                        , H.div [ A.class "col-9" ]
                            [ H.input
                                [ A.id (widgetId BackgroundImageField)
                                , A.type_ "text"
                                , A.value value_
                                , A.placeholder ""
                                , A.autocomplete False
                                , A.class "form-control form-control-sm"
                                , E.onFocus (FieldEditingStarted BackgroundImageField value_)
                                , E.onBlur FieldEditingFinished
                                , E.onInput FieldChanged
                                ]
                                []
                            ]
                        ]
                    ]

            Background.Solid value ->
                H.div []
                    [ backgroundToggleView model background
                    , colorView model (Just value) BackgroundColorField BackgroundColorChanged
                    ]

            Background.None ->
                none
        ]


backgroundToggleView model background =
    H.div [ A.class "btn-group w-100 mb-2", A.attribute "role" "group" ]
        [ H.button
            [ A.classList
                [ ( "btn btn-secondary btn-sm", True )
                , ( "active", Background.isSolid background )
                ]
            , E.onClick (BackgroundChanged (Background.Solid Palette.lightGray))
            , A.type_ "button"
            ]
            [ H.text "Color" ]
        , H.button
            [ A.classList
                [ ( "btn btn-secondary btn-sm", True )
                , ( "active", Background.isImage background )
                ]
            , E.onClick (BackgroundChanged (Background.Image ""))
            , A.type_ "button"
            ]
            [ H.text "Image" ]
        ]



-- backgroundSizingView : Model -> Background -> Html Msg
-- backgroundSizingView model value =
--     if value == Background.None then
--         none
--     else
--         let
--             url =
--                 backgroundImageUrl value
--         in
--         H.div [ A.class "row align-items-center mb-2" ]
--             [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
--                 [ H.text "Sizing" ]
--             , H.div [ A.class "col-9 btn-group", A.attribute "role" "group" ]
--                 [ H.button
--                     [ A.classList
--                         [ ( "btn btn-light btn-sm w-33", True )
--                         , ( "active", isCropped value )
--                         ]
--                     , A.type_ "button"
--                     , A.title "Fit the containing element by cropping the image"
--                     , E.onClick (BackgroundChanged (Background.Cropped url))
--                     ]
--                     [ H.text "Crop" ]
--                 , H.button
--                     [ A.classList
--                         [ ( "btn btn-light btn-sm w-33", True )
--                         , ( "active", isUncropped value )
--                         ]
--                     , A.type_ "button"
--                     , A.title "Fit the containing element by scaling the image"
--                     , E.onClick (BackgroundChanged (Background.Uncropped url))
--                     ]
--                     [ H.text "Uncrop" ]
--                 , H.button
--                     [ A.classList
--                         [ ( "btn btn-light btn-sm w-33", True )
--                         , ( "active", isTiled value )
--                         ]
--                     , A.type_ "button"
--                     , A.title "Tile the image along the X and Y axes"
--                     , E.onClick (BackgroundSizingChanged (Background.Tiled url))
--                     ]
--                     [ H.text "Tile" ]
--                 ]
--             ]
-- backgroundImageUrl : Background -> String
-- backgroundImageUrl value =
--     case value of
--         Background.Image value_ ->
--             value_
--         _ ->
--             ""
-- isCropped : Background -> Bool
-- isCropped value =
--     case value of
--         Background.Cropped _ ->
--             True
--         _ ->
--             False
-- isUncropped value =
--     case value of
--         Background.Uncropped _ ->
--             True
--         _ ->
--             False
-- isTiled value =
--     case value of
--         Background.Tiled _ ->
--             True
--         _ ->
--             False


lengthView : Model -> Node -> Html Msg
lengthView model node =
    H.div [ A.class "mb-3" ]
        [ widthView model node
        , heightView model node
        ]


pageLengthView : Model -> Node -> Html Msg
pageLengthView model node =
    H.div [ A.class "mb-3" ]
        [ presetSizeView model node
        , pageWidthView model node
        , pageHeightView model node
        ]


presetSizeView : Model -> Node -> Html Msg
presetSizeView model node =
    H.div [ A.class "row align-items-center mb-2" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Preset" ]
        , H.div [ A.class "col-9" ]
            [ H.select [ onViewportSelect PresetSizeChanged, A.class "form-select form-select-sm" ]
                (H.option [] [ H.text "Custom" ]
                    :: List.map
                        (\viewport ->
                            case viewport of
                                Device name width height orientation ->
                                    let
                                        label =
                                            name
                                                ++ Entity.ensp
                                                ++ String.fromInt width
                                                ++ Entity.times
                                                ++ String.fromInt height
                                                ++ "px"

                                        --++ Entity.ensp
                                        --++ orientationLabel orientation
                                    in
                                    H.option [ A.selected (node.width == Px width && node.heightMin == Just height), viewportValue viewport ]
                                        [ H.text label ]

                                _ ->
                                    -- Should not happen
                                    none
                        )
                        Document.devices
                )
            ]
        ]


viewportValue : Viewport -> Attribute msg
viewportValue value =
    A.value (Codecs.encodeViewport value)


onViewportSelect msg =
    E.on "input" (Codecs.viewportDecoder msg)


orientationLabel : Orientation -> String
orientationLabel value =
    case value of
        Portrait ->
            "Portrait"

        Landscape ->
            "Landscape"


wrapRowOptionView : Bool -> Html Msg
wrapRowOptionView wrapped =
    H.div [ A.class "form-check form-switch" ]
        [ H.input
            [ E.onCheck WrapRowItemsChanged
            , A.checked wrapped
            , A.class "form-check-input"
            , A.id "wrap-row-items"
            , A.type_ "checkbox"
            ]
            []
        , H.label
            [ A.class "form-check-label"
            , A.for "wrap-row-items"
            ]
            [ H.text "Wrap row items" ]
        ]


widthView : Model -> Node -> Html Msg
widthView model ({ width, widthMin, widthMax } as node) =
    let
        min =
            case model.inspector of
                EditingField WidthMinField new ->
                    new

                _ ->
                    Maybe.map String.fromInt widthMin
                        |> Maybe.withDefault ""

        max =
            case model.inspector of
                EditingField WidthMaxField new ->
                    new

                _ ->
                    Maybe.map String.fromInt widthMax
                        |> Maybe.withDefault ""

        addFitButton : List (Html Msg) -> Html Msg
        addFitButton buttons =
            H.div [ A.class "btn-group w-100 mb-1", A.attribute "role" "group" ]
                (if not (Document.isImageNode node) then
                    H.button
                        [ A.classList
                            [ ( "btn btn-secondary btn-sm", True )
                            , ( "active", isContent width )
                            ]
                        , E.onClick (WidthChanged Layout.fit)
                        , A.type_ "button"
                        ]
                        [ H.text "Fit" ]
                        :: buttons

                 else
                    buttons
                )
    in
    H.div [ A.class "row align-items-center mb-3" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Width" ]
        , H.div [ A.class "col-9" ]
            [ addFitButton
                [ case width of
                    Fill value ->
                        H.button
                            [ A.classList
                                [ ( "btn btn-secondary btn-sm", True )
                                , ( "active", True )
                                ]
                            , E.onClick (WidthChanged (Layout.portion value))
                            , A.type_ "button"
                            ]
                            [ H.text "Fill" ]

                    _ ->
                        H.button
                            [ A.classList
                                [ ( "btn btn-secondary btn-sm", True )
                                ]
                            , E.onClick (WidthChanged Layout.fill)
                            , A.type_ "button"
                            ]
                            [ H.text "Fill" ]
                , case width of
                    Px value ->
                        H.button
                            [ A.classList
                                [ ( "btn btn-secondary btn-sm", True )
                                , ( "active", isPxOrUnspecified width )
                                ]
                            , E.onClick (WidthChanged (Layout.px value))
                            , A.type_ "button"
                            ]
                            [ H.text "Px" ]

                    _ ->
                        H.button
                            [ A.classList
                                [ ( "btn btn-secondary btn-sm", True )
                                , ( "active", isPxOrUnspecified width )
                                ]
                            , E.onClick (WidthChanged Layout.unspecified)
                            , A.type_ "button"
                            ]
                            [ H.text "Px" ]
                ]
            , H.div [ A.class "d-flex justify-content-end", A.style "gap" ".25rem" ]
                (case width of
                    Px value ->
                        let
                            value_ =
                                case model.inspector of
                                    EditingField WidthPxField new ->
                                        new

                                    _ ->
                                        String.fromInt value
                        in
                        [ numericFieldView WidthPxField "Exact" value_
                        , numericFieldView WidthMinField "Min." min
                        , numericFieldView WidthMaxField "Max." max
                        ]

                    Unspecified ->
                        let
                            value_ =
                                case model.inspector of
                                    EditingField WidthPxField new ->
                                        new

                                    _ ->
                                        ""
                        in
                        [ numericFieldView WidthPxField "Exact" value_
                        , numericFieldView WidthMinField "Min." min
                        , numericFieldView WidthMaxField "Max." max
                        ]

                    Content ->
                        [ numericFieldView WidthMinField "Min." min
                        , numericFieldView WidthMaxField "Max." max
                        ]

                    Fill value ->
                        let
                            value_ =
                                case model.inspector of
                                    EditingField WidthPortionField new ->
                                        new

                                    _ ->
                                        String.fromInt value
                        in
                        [ numericFieldView WidthPortionField "Portion" value_
                        , numericFieldView WidthMinField "Min." min
                        , numericFieldView WidthMaxField "Max." max
                        ]
                )
            ]
        ]


pageWidthView : Model -> Node -> Html Msg
pageWidthView model { width, widthMin, widthMax } =
    let
        min =
            case model.inspector of
                EditingField WidthMinField new ->
                    new

                _ ->
                    Maybe.map String.fromInt widthMin
                        |> Maybe.withDefault ""

        -- max =
        --     case model.inspector of
        --         EditingField WidthMaxField new ->
        --             new
        --         _ ->
        --             Maybe.map String.fromInt widthMax
        --                 |> Maybe.withDefault ""
    in
    H.div [ A.class "row align-items-center mb-3" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Width" ]
        , H.div [ A.class "col-9" ]
            [ H.div [ A.class "d-flex justify-content-end", A.style "gap" ".25rem" ]
                (case width of
                    Px value ->
                        let
                            value_ =
                                case model.inspector of
                                    EditingField WidthPxField new ->
                                        new

                                    _ ->
                                        String.fromInt value
                        in
                        [ numericFieldView WidthPxField "Exact" value_
                        , numericFieldView WidthMinField "Min." min

                        --, numericFieldView WidthMaxField "Max." max
                        ]

                    Unspecified ->
                        let
                            value_ =
                                case model.inspector of
                                    EditingField WidthPxField new ->
                                        new

                                    _ ->
                                        ""
                        in
                        [ numericFieldView WidthPxField "Exact" value_
                        , numericFieldView WidthMinField "Min." min

                        --, numericFieldView WidthMaxField "Max." max
                        ]

                    _ ->
                        []
                )
            ]
        ]


heightView : Model -> Node -> Html Msg
heightView model ({ height, heightMin, heightMax } as node) =
    let
        min =
            case model.inspector of
                EditingField HeightMinField new ->
                    new

                _ ->
                    Maybe.map String.fromInt heightMin
                        |> Maybe.withDefault ""

        max =
            case model.inspector of
                EditingField HeightMaxField new ->
                    new

                _ ->
                    Maybe.map String.fromInt heightMax
                        |> Maybe.withDefault ""

        addFitButton : List (Html Msg) -> Html Msg
        addFitButton buttons =
            H.div [ A.class "btn-group w-100 mb-1", A.attribute "role" "group" ]
                (if not (Document.isImageNode node) then
                    H.button
                        [ A.classList
                            [ ( "btn btn-secondary btn-sm", True )
                            , ( "active", isContent height )
                            ]
                        , E.onClick (HeightChanged Layout.fit)
                        , A.type_ "button"
                        ]
                        [ H.text "Fit" ]
                        :: buttons

                 else
                    buttons
                )
    in
    H.div [ A.class "row align-items-center  mb-3" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Height" ]
        , H.div [ A.class "col-9" ]
            [ addFitButton
                [ case height of
                    Fill value ->
                        H.button
                            [ A.classList
                                [ ( "btn btn-secondary btn-sm", True )
                                , ( "active", True )
                                ]
                            , E.onClick (HeightChanged (Layout.portion value))
                            , A.type_ "button"
                            ]
                            [ H.text "Fill" ]

                    _ ->
                        H.button
                            [ A.classList
                                [ ( "btn btn-secondary btn-sm", True )
                                ]
                            , E.onClick (HeightChanged Layout.fill)
                            , A.type_ "button"
                            ]
                            [ H.text "Fill" ]
                , case height of
                    Px value ->
                        H.button
                            [ A.classList
                                [ ( "btn btn-secondary btn-sm", True )
                                , ( "active", isPxOrUnspecified height )
                                ]
                            , E.onClick (HeightChanged (Layout.px value))
                            , A.type_ "button"
                            ]
                            [ H.text "Px" ]

                    _ ->
                        H.button
                            [ A.classList
                                [ ( "btn btn-secondary btn-sm", True )
                                , ( "active", isPxOrUnspecified height )
                                ]
                            , E.onClick (HeightChanged Layout.unspecified)
                            , A.type_ "button"
                            ]
                            [ H.text "Px" ]
                ]
            , H.div [ A.class "d-flex justify-content-end", A.style "gap" ".25rem" ]
                (case height of
                    Px value ->
                        let
                            value_ =
                                case model.inspector of
                                    EditingField HeightPxField new ->
                                        new

                                    _ ->
                                        String.fromInt value
                        in
                        [ numericFieldView HeightPxField "Exact" value_
                        , numericFieldView HeightMinField "Min." min
                        , numericFieldView HeightMaxField "Max." max
                        ]

                    Unspecified ->
                        let
                            value_ =
                                case model.inspector of
                                    EditingField HeightPxField new ->
                                        new

                                    _ ->
                                        ""
                        in
                        [ numericFieldView HeightPxField "Exact" value_
                        , numericFieldView HeightMinField "Min." min
                        , numericFieldView HeightMaxField "Max." max
                        ]

                    Content ->
                        [ numericFieldView HeightMinField "Min." min
                        , numericFieldView HeightMaxField "Max." max
                        ]

                    Fill value ->
                        let
                            value_ =
                                case model.inspector of
                                    EditingField HeightPortionField new ->
                                        new

                                    _ ->
                                        String.fromInt value
                        in
                        [ numericFieldView HeightPortionField "Portion" value_
                        , numericFieldView HeightMinField "Min." min
                        , numericFieldView HeightMaxField "Max." max
                        ]
                )
            ]
        ]


pageHeightView : Model -> Node -> Html Msg
pageHeightView model { height, heightMin, heightMax } =
    let
        min =
            case model.inspector of
                EditingField HeightMinField new ->
                    new

                _ ->
                    Maybe.map String.fromInt heightMin
                        |> Maybe.withDefault ""

        -- max =
        --     case model.inspector of
        --         EditingField HeightMaxField new ->
        --             new
        --         _ ->
        --             Maybe.map String.fromInt heightMax
        --                 |> Maybe.withDefault ""
    in
    H.div []
        [ H.div [ A.class "row align-items-center  mb-3" ]
            [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
                [ H.text "Height" ]
            , H.div [ A.class "col-9" ]
                [ H.div [ A.class "d-flex justify-content-end", A.style "gap" ".25rem" ]
                    (case height of
                        Px value ->
                            let
                                value_ =
                                    case model.inspector of
                                        EditingField HeightPxField new ->
                                            new

                                        _ ->
                                            String.fromInt value
                            in
                            [ numericFieldView HeightPxField "Exact" value_
                            , numericFieldView HeightMinField "Min." min

                            --, numericFieldView HeightMaxField "Max." max
                            ]

                        Unspecified ->
                            let
                                value_ =
                                    case model.inspector of
                                        EditingField HeightPxField new ->
                                            new

                                        _ ->
                                            ""
                            in
                            [ numericFieldView HeightPxField "Exact" value_
                            , numericFieldView HeightMinField "Min." min

                            --, numericFieldView HeightMaxField "Max." max
                            ]

                        _ ->
                            []
                    )
                ]
            ]
        ]


imageView : ImageData -> Model -> Node -> Html Msg
imageView image model node =
    let
        width =
            Maybe.map String.fromInt image.width
                |> Maybe.withDefault ""

        height =
            Maybe.map String.fromInt image.height
                |> Maybe.withDefault ""
    in
    H.div []
        [ H.div [ A.class "row align-items-center mb-2" ]
            [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
                [ H.text "Format" ]
            , H.div [ A.class "col-9" ]
                [ H.div [ A.class "small" ]
                    [ H.text (mimeTypeLabel image.mimeType)
                    , H.text Entity.nbsp
                    , H.text Entity.middot
                    , H.text Entity.nbsp
                    , H.text width
                    , H.text Entity.times
                    , H.text height
                    , H.text "px"
                    ]
                ]
            ]
        , H.div [ A.class "row align-items-center" ]
            [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
                [ H.text "URL" ]
            , H.div [ A.class "col-9" ]
                [ H.div [ A.class "input-group" ]
                    [ H.input
                        [ A.id (widgetId ImageSrcField)
                        , A.type_ "text"
                        , A.value image.src
                        , A.class "form-control form-control-sm"
                        , A.readonly True
                        ]
                        []
                    , H.button
                        [ A.class "btn btn-sm btn-secondary"
                        , A.type_ "button"
                        , E.onClick (ClipboardCopyClicked image.src)
                        ]
                        [ H.text "Copy"
                        ]
                    ]
                ]
            ]
        ]


mimeTypeLabel : Maybe String -> String
mimeTypeLabel value =
    case value of
        Just "image/webp" ->
            "WebP"

        Just "image/jpeg" ->
            "JPEG"

        Just "image/png" ->
            "PNG"

        Just "image/gif" ->
            "GIF"

        Just "image/svg+xml" ->
            "SVG"

        _ ->
            "Unknown"


isContent value =
    case value of
        Content ->
            True

        _ ->
            False


isPxOrUnspecified value =
    case value of
        Px _ ->
            True

        Unspecified ->
            True

        _ ->
            False


alignmentView : Model -> Node -> Html Msg
alignmentView model ({ offsetX, offsetY } as node) =
    let
        offsetX_ =
            case model.inspector of
                EditingField OffsetXField new ->
                    new

                _ ->
                    String.fromFloat offsetX

        offsetY_ =
            case model.inspector of
                EditingField OffsetYField new ->
                    new

                _ ->
                    String.fromFloat offsetY
    in
    H.div [ A.class "row align-items-center mb-3" ]
        [ H.label [ A.class "col-3 col-form-label-sm" ]
            [ H.text "Alignment" ]
        , H.div [ A.class "col-9" ]
            [ H.div [ A.class "d-flex align-items-center mb-1" ]
                [ alignmentView_ model node
                , H.div
                    ([ A.class "w-33 ms-1"
                     ]
                        |> Common.addTooltipLeft "Move right/left"
                    )
                    [ H.input
                        [ A.id (widgetId OffsetXField)
                        , A.class "form-control form-control-sm text-center mx-auto"
                        , A.type_ "number"
                        , A.value offsetX_
                        , E.onFocus (FieldEditingStarted OffsetXField offsetX_)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                ]
            , H.div
                ([ A.class "me-1"
                 ]
                    |> Common.addTooltipDown "Move down/up"
                )
                [ H.input
                    [ A.id (widgetId OffsetYField)
                    , A.class "form-control form-control-sm text-center mx-auto w-33"
                    , A.type_ "number"
                    , A.value offsetY_
                    , E.onFocus (FieldEditingStarted OffsetYField offsetY_)
                    , E.onBlur FieldEditingFinished
                    , E.onInput FieldChanged
                    ]
                    []
                ]
            ]
        ]


positionView : Model -> Node -> Html Msg
positionView model { type_, position } =
    let
        setSelected other attrs =
            A.selected (position == other) :: attrs
    in
    H.div [ A.class "row align-items-center mb-3" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Position" ]
        , H.div [ A.class "col-9" ]
            [ Keyed.node "select"
                [ onPositionSelect PositionChanged, A.class "form-select form-select-sm" ]
                (List.map
                    (\position_ ->
                        let
                            name =
                                Layout.positionName position_
                                    |> Maybe.withDefault "Normal"
                        in
                        ( name
                        , H.option (setSelected position_ [ positionValue position_ ])
                            [ H.text name ]
                        )
                    )
                    positions
                )
            ]
        ]


positionValue : Position -> Attribute msg
positionValue value =
    A.value (Codecs.encodePosition value)


onPositionSelect msg =
    E.on "input" (Codecs.positionDecoder msg)


positions =
    [ Normal, Above, Below, OnStart, OnEnd, InFront, BehindContent ]


alignmentView_ : Model -> Node -> Html Msg
alignmentView_ _ { alignmentX, alignmentY } =
    let
        nextAlignLeft =
            nextAlignStartState alignmentX

        nextAlignRight =
            nextAlignEndState alignmentX

        nextAlignTop =
            nextAlignStartState alignmentY

        nextAlignBottom =
            nextAlignEndState alignmentY

        nextAlign =
            if alignmentX == None && alignmentY == None then
                Center

            else
                None
    in
    H.div [ A.class "bg-white border rounded ms-auto w-33" ]
        -- Top align
        [ H.div [ A.class "d-flex justify-content-center" ]
            [ H.button
                [ A.classList
                    [ ( "bp-0 border-0 bg-white lh-1 align-off", True )
                    , ( "align-on", alignmentY == Start || alignmentY == Center )
                    ]
                , E.onClick (AlignmentYChanged nextAlignTop)
                ]
                [ Icons.pipe ]
            ]
        , H.div [ A.class "d-flex align-items-center justify-content-between" ]
            -- Left align
            [ H.button
                [ A.classList
                    [ ( "rotate-90 bp-0 border-0 bg-white lh-1 align-off", True )
                    , ( "align-on", alignmentX == Start || alignmentX == Center )
                    ]
                , E.onClick (AlignmentXChanged nextAlignLeft)
                ]
                [ Icons.pipe ]
            , H.button
                [ A.class "bg-light border rounded"
                , A.style "width" "1.5rem"
                , A.style "height" "1.5rem"
                , E.onClick (AlignmentChanged nextAlign)
                ]
                []

            -- Right align
            , H.button
                [ A.classList
                    [ ( "rotate-90 bp-0 border-0 bg-white lh-1 align-off", True )
                    , ( "align-on", alignmentX == End || alignmentX == Center )
                    ]
                , E.onClick (AlignmentXChanged nextAlignRight)
                ]
                [ Icons.pipe ]
            ]

        -- Bottom align
        , H.div [ A.class "d-flex justify-content-center" ]
            [ H.button
                [ A.classList
                    [ ( "bp-0 border-0 bg-white lh-1 align-off", True )
                    , ( "align-on", alignmentY == End || alignmentY == Center )
                    ]
                , E.onClick (AlignmentYChanged nextAlignBottom)
                ]
                [ Icons.pipe ]
            ]
        ]


nextAlignStartState value =
    case value of
        Start ->
            None

        Center ->
            End

        End ->
            Center

        None ->
            Start


nextAlignEndState value =
    case value of
        Start ->
            Center

        Center ->
            Start

        End ->
            None

        None ->
            End


textAlignmentView : Model -> TextAlignment -> Html Msg
textAlignmentView _ value =
    H.div [ A.class "row align-items-center mb-0" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Alignment" ]
        , H.div [ A.class "col-9 btn-group", A.attribute "role" "group" ]
            [ H.button
                [ A.classList
                    [ ( "btn btn-secondary btn-sm w-25", True )
                    , ( "active", value == TextStart )
                    ]
                , E.onClick (TextAlignChanged TextStart)
                , A.type_ "button"
                ]
                [ Icons.alignLeft ]
            , H.button
                [ A.classList
                    [ ( "btn btn-secondary btn-sm w-25", True )
                    , ( "active", value == TextCenter )
                    ]
                , E.onClick (TextAlignChanged TextCenter)
                , A.type_ "button"
                ]
                [ Icons.alignCenter ]
            , H.button
                [ A.classList
                    [ ( "btn btn-secondary btn-sm w-25", True )
                    , ( "active", value == TextEnd )
                    ]
                , E.onClick (TextAlignChanged TextEnd)
                , A.type_ "button"
                ]
                [ Icons.alignRight ]
            , H.button
                [ A.classList
                    [ ( "btn btn-secondary btn-sm w-25", True )
                    , ( "active", value == TextJustify )
                    ]
                , E.onClick (TextAlignChanged TextJustify)
                , A.type_ "button"
                ]
                [ Icons.alignJustify ]
            ]
        ]


fontView : Model -> Zipper Node -> Html Msg
fontView model zipper =
    let
        node =
            Zipper.label zipper

        theme =
            Theme.defaultTheme

        ( inherited, resolvedFontSize ) =
            case model.inspector of
                EditingField FontSizeField new ->
                    ( False, new )

                _ ->
                    case node.fontSize of
                        Local value ->
                            ( False, String.fromInt value )

                        Inherited ->
                            ( True
                            , Document.resolveInheritedFontSize theme.textSize zipper
                                |> String.fromInt
                            )

        resolvedFontFamily =
            Document.resolveInheritedFontFamily theme.textFontFamily zipper

        resolvedFontColor =
            case node.fontColor of
                Local value ->
                    value

                Inherited ->
                    Document.resolveInheritedFontColor theme.textColor zipper
    in
    H.div []
        [ H.div [ A.class "mb-2" ]
            [ fontFamilyView node.fontFamily resolvedFontFamily (canInherit node)
            ]
        , H.div [ A.class "d-flex" ]
            [ H.div [ A.class "mb-2 me-1 w-25" ]
                [ H.input
                    [ A.id (widgetId FontSizeField)
                    , A.classList
                        [ ( "form-control form-control-sm text-center", True )
                        , ( "text-muted fst-italic", inherited )
                        ]
                    , A.type_ "number"
                    , A.min (String.fromInt Font.minFontSizeAllowed)
                    , A.value resolvedFontSize
                    , E.onFocus (FieldEditingStarted FontSizeField resolvedFontSize)
                    , E.onBlur FieldEditingFinished
                    , E.onInput FieldChanged
                    ]
                    []
                    |> addDropdown FontSizeField model.dropDownState (fontSizeItems node)
                ]
            , H.div [ A.class "mb-2 w-75" ]
                [ fontWeightView resolvedFontFamily node.fontWeight
                ]
            ]
        , colorView model (Just resolvedFontColor) FontColorField FontColorChanged
        ]


{-| Classic typographic scale.

    See https://www.kevinpowell.co/article/typographic-scale/

-}
fontSizes =
    [ 11, 12, 14, 16, 18, 24, 30, 36, 48, 60, 72 ]
        |> List.map String.fromInt


fontSizeItems node =
    (if canInherit node then
        H.div [ E.onClick (FontSizeChanged "inherit"), A.class "dropdown-item" ]
            [ H.text "Inherit" ]

     else
        none
    )
        :: List.map
            (\value ->
                H.div [ E.onClick (FontSizeChanged value), A.class "dropdown-item text-center unselectable" ]
                    [ H.text value ]
            )
            fontSizes


fontSpacingView model node =
    let
        lineSpacing =
            case node.spacing of
                Spacing ( _, y ) ->
                    case model.inspector of
                        EditingField SpacingYField new ->
                            new

                        _ ->
                            String.fromInt y

                SpaceEvenly ->
                    -- TODO figure out if it makes sense here
                    ""

        wordSpacing =
            case model.inspector of
                EditingField WordSpacingField new ->
                    new

                _ ->
                    String.fromFloat node.wordSpacing

        letterSpacing =
            case model.inspector of
                EditingField LetterSpacingField new ->
                    new

                _ ->
                    String.fromFloat node.letterSpacing
    in
    H.div [ A.class "mb-2 row align-items-center" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Spacing"
            ]
        , H.div [ A.class "col-9" ]
            [ H.div [ A.class "d-flex justify-content-end", A.style "gap" ".25rem" ]
                [ case node.type_ of
                    ParagraphNode _ ->
                        numericFieldView SpacingYField "Line" lineSpacing

                    HeadingNode _ ->
                        numericFieldView SpacingYField "Line" lineSpacing

                    _ ->
                        none
                , numericFieldView WordSpacingField "Word" wordSpacing
                , numericFieldView LetterSpacingField "Letter" letterSpacing
                ]
            ]
        ]


fontFamilyView : Local FontFamily -> FontFamily -> Bool -> Html Msg
fontFamilyView fontFamily resolvedFontFamily inherit =
    let
        setSelected other attrs =
            case fontFamily of
                Local family ->
                    A.selected (family.name == other) :: attrs

                _ ->
                    A.selected False :: attrs

        inheritOption =
            case fontFamily of
                Local _ ->
                    if inherit then
                        ( "inherit"
                        , H.option [ fontFamilyValue Inherited ]
                            [ H.text "Inherit" ]
                        )

                    else
                        ( "", none )

                Inherited ->
                    -- Generate a unique enough key to avoid VDOM quirks
                    ( "inherited-" ++ resolvedFontFamily.name
                    , H.option [ A.disabled True, A.selected True ]
                        [ H.text resolvedFontFamily.name ]
                    )
    in
    Keyed.node "select"
        [ onFontFamilySelect FontFamilyChanged
        , A.classList
            [ ( "form-select form-select-sm", True )
            , ( "text-muted fst-italic", fontFamily == Inherited )
            ]
        ]
        (inheritOption
            :: (Fonts.families
                    |> List.map
                        (\( group, families ) ->
                            ( group
                            , Keyed.node "optgroup"
                                [ A.attribute "label" group ]
                                (List.map
                                    (\family ->
                                        ( family.name
                                        , H.option (setSelected family.name [ fontFamilyValue (Local family) ])
                                            [ H.text family.name ]
                                        )
                                    )
                                    families
                                )
                            )
                        )
               )
        )


fontFamilyValue : Local FontFamily -> Attribute msg
fontFamilyValue value =
    A.value (Codecs.encodeFontFamily value)


onFontFamilySelect msg =
    E.on "input" (Codecs.fontFamilyDecoder msg)


fontWeightView : FontFamily -> FontWeight -> Html Msg
fontWeightView fontFamily fontWeight =
    let
        setSelected other attrs =
            A.selected (fontWeight == other) :: attrs
    in
    Keyed.node "select"
        [ onFontWeightSelect FontWeightChanged, A.class "form-select form-select-sm" ]
        (List.map
            (\weight ->
                let
                    name =
                        Font.weightName weight
                in
                ( name
                , H.option (setSelected weight [ fontWeightValue weight ])
                    [ H.text name ]
                )
            )
            fontFamily.weights
        )


fontWeightValue : FontWeight -> Attribute msg
fontWeightValue value =
    A.value (Codecs.encodeFontWeight value)


onFontWeightSelect msg =
    E.on "input" (Codecs.fontWeightDecoder msg)


canInherit node =
    not (Document.isPageNode node)


emptyView : Model -> a -> Html Msg
emptyView _ _ =
    H.div [] []


numericFieldView : Widget -> String -> String -> Html Msg
numericFieldView field label value =
    H.div [ A.class "w-33" ]
        [ H.label [ A.class "col-form-label-sm m-0 p-0", A.for (widgetId field) ]
            [ H.text label
            ]
        , H.input
            [ A.id (widgetId field)
            , A.class "form-control form-control-sm text-center"
            , A.type_ "number"
            , A.min "0"
            , A.value value
            , E.onFocus (FieldEditingStarted field value)
            , E.onBlur FieldEditingFinished
            , E.onInput FieldChanged
            ]
            []
        ]


addStyleButtonView : Msg -> Html Msg
addStyleButtonView msg =
    H.button
        ([ A.type_ "button"
         , A.class "btn btn-transparent bp-0"
         , E.onClick msg
         ]
         --|> Common.addTooltipLeft "Remove style"
        )
        [ Icons.plusSmall ]


removeStyleButtonView : Msg -> Html Msg
removeStyleButtonView msg =
    H.button [ A.type_ "button", A.class "btn btn-transparent bp-0", E.onClick msg ] [ Icons.trashSmall ]
