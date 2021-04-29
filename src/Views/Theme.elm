module Views.Theme exposing (..)

{- Theme editing. -}

import Codecs
import Dict exposing (Dict)
import Document exposing (..)
import Element exposing (Color, Orientation(..))
import Element.Background exposing (image)
import Fonts
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Entity as Entity
import Html.Events as E
import Html.Keyed as Keyed
import Icons
import Model exposing (..)
import SelectList exposing (SelectList)
import Style.Background as Background exposing (Background)
import Style.Font as Font exposing (..)
import Style.Layout as Layout exposing (..)
import Style.Theme as Theme exposing (Theme)
import Views.Common as Common


view : Model -> List (Html Msg)
view model =
    [ H.form [ E.onSubmit FieldEditingConfirmed ]
        -- This makes onSubmit to work when hitting enter
        (H.button [ A.type_ "submit", A.class "sr-only" ] []
            :: themeViews model
        )
    ]


themeViews : Model -> List (Html Msg)
themeViews model =
    let
        theme =
            model.theme

        textSettings =
            { size = theme.textSize
            , color = theme.textColor
            , family = theme.textFontFamily
            , weight = theme.textFontWeight
            , onSizeChange = ThemeTextSizeChanged 
            , onColorChange = ThemeTextColorChanged
            , onFamilyChange = ThemeTextFontFamilyChanged
            , onWeightChange = ThemeTextFontWeightChanged
            , sizeField = ThemeTextFontSizeField
            , colorField = ThemeTextColorField
            }
        
        --headingSettings = 

        
    in
    [ sectionView "Page defaults"
        [ fontView model textSettings

        --, backgroundView model
        ]
    , sectionView "Colors"
        []

    --, bordersView model
    , sectionView "Headings"
        [ fontView model textSettings
        ]
    ]



-- commonViews zipper model node =
--     [ sectionView "Layout"
--         [ positionView model node
--         --, lengthView model node
--         , paddingView model node
--         ]
--     , sectionView "Text"
--         [ fontView model zipper
--         , fontSpacingView model node
--         , textAlignmentView model node.textAlignment
--         ]
--     , bordersView model node
--     , backgroundView model node
--     ]


sectionView : String -> List (Html Msg) -> Html Msg
sectionView title views =
    H.section [ A.class "section bp-3 border-bottom" ]
        ((if String.isEmpty title then
            Common.none

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
    H.div [ A.class "form-group row align-items-center mb-2" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0" ]
            [ H.text "Text" ]
        , H.div [ A.class "col-9" ]
            [ H.input
                [ A.id (Common.fieldId LabelField)
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
    H.div [ A.class "form-group row align-items-center mb-2" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Spacing X" ]
        , H.div [ A.class "col-9" ]
            [ H.input
                [ A.class "form-control form-control-sm"
                , A.type_ "number"
                , A.min "0"
                , A.value x

                --, A.title "Space between row items"
                , E.onFocus (FieldEditingStarted SpacingXField x)
                , E.onBlur FieldEditingFinished
                , E.onInput FieldChanged
                ]
                []
            ]
        ]


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
    H.div [ A.class "form-group row align-items-center mb-2" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Spacing Y" ]
        , H.div [ A.class "col-9" ]
            [ H.input
                [ A.class "form-control form-control-sm"
                , A.type_ "number"
                , A.min "0"
                , A.value y

                --, A.title "Space between column items"
                , E.onFocus (FieldEditingStarted SpacingYField y)
                , E.onBlur FieldEditingFinished
                , E.onInput FieldChanged
                ]
                []
            ]
        ]


addDropdown fieldId_ state items parent =
    let
        visible =
            case state of
                Visible id ->
                    id == fieldId_

                Hidden ->
                    False
    in
    H.div [ A.class "input-group" ]
        [ parent
        , H.div [ A.class "input-group-append" ]
            [ H.button
                [ --A.attribute "aria-expanded" "false"
                  A.attribute "aria-haspopup" "true"
                , A.class "btn btn-light btn-sm dropdown-toggle"
                , E.onClick
                    (DropDownChanged
                        (if visible then
                            Hidden

                         else
                            Visible fieldId_
                        )
                    )

                --, A.attribute "data-toggle" "dropdown"
                , A.type_ "button"
                ]
                [ H.text "" ]
            , H.div
                [ A.classList
                    [ ( "dropdown-menu", True )
                    , ( Common.fieldId fieldId_ ++ "-dropdown", True )
                    , ( "show", visible )
                    ]
                ]
                items

            -- , H.div [ A.class "dropdown-divider", A.attribute "role" "separator" ]
            --     []
            ]
        ]


bordersView : Model -> Node -> Html Msg
bordersView model { borderColor, borderWidth, borderCorner } =
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
    H.section [ A.class "section bp-3  border-bottom" ]
        [ H.h2 [ A.class "section__title mb-2" ]
            [ H.text "Border" ]
        , H.div [ A.class "form-group row align-items-center mb-2" ]
            [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
                [ H.text "Size" ]
            , H.div [ A.class "col-9" ]
                [ H.div [ A.class "d-flex justify-content-between mb-1" ]
                    [ H.div [ A.class "w-25 mr-1" ]
                        [ H.div [ A.class "input-group input-group-sm" ]
                            [ H.div [ A.class "input-group-prepend" ]
                                [ H.span [ A.class "input-group-text bpx-1" ] [ H.text Entity.ulcorner ]
                                ]
                            , H.input
                                [ A.id (Common.fieldId BorderTopLeftCornerField)
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
                    , H.div [ A.class "w-25 mr-1" ]
                        [ H.input
                            [ A.id (Common.fieldId BorderTopWidthField)
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
                                [ A.id (Common.fieldId BorderTopRightCornerField)
                                , A.type_ "number"
                                , A.min "0"
                                , A.value topRightCorner
                                , A.class "form-control form-control-sm text-center"
                                , E.onFocus (FieldEditingStarted BorderTopRightCornerField topRightCorner)
                                , E.onBlur FieldEditingFinished
                                , E.onInput FieldChanged
                                ]
                                []
                            , H.div [ A.class "input-group-append" ]
                                [ H.span [ A.class "input-group-text bpx-1" ] [ H.text Entity.urcorner ]
                                ]
                            ]
                        ]
                    ]
                , H.div [ A.class "d-flex justify-content-between align-items-center mb-1" ]
                    [ H.div [ A.class "w-25" ]
                        [ H.input
                            [ A.id (Common.fieldId BorderLeftWidthField)
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
                            [ A.classList
                                [ ( "btn btn-link", True )
                                , ( "text-dark", not borderWidth.locked )
                                ]
                            , E.onClick (BorderLockChanged (not borderWidth.locked))
                            , A.title "Use a single border for all directions"
                            , A.type_ "button"
                            ]
                            [ if borderWidth.locked then
                                Icons.lock

                              else
                                Icons.unlock
                            ]
                        ]
                    , H.div [ A.class "w-25" ]
                        [ H.input
                            [ A.id (Common.fieldId BorderRightWidthField)
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
                    [ H.div [ A.class "w-25 mr-1" ]
                        [ H.div [ A.class "input-group input-group-sm" ]
                            [ H.div [ A.class "input-group-prepend" ]
                                [ H.span [ A.class "input-group-text bpx-1" ] [ H.text Entity.llcorner ]
                                ]
                            , H.input
                                [ A.id (Common.fieldId BorderBottomLeftCornerField)
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
                    , H.div [ A.class "w-25 mr-1" ]
                        [ H.input
                            [ A.id (Common.fieldId BorderBottomWidthField)
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
                                [ A.id (Common.fieldId BorderBottomRightCornerField)
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
                            , H.div [ A.class "input-group-append" ]
                                [ H.span [ A.class "input-group-text bpx-1" ] [ H.text Entity.lrcorner ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , Common.colorView model (Just borderColor) BorderColorField BorderColorChanged
        ]


fontView :
    Model
    ->
        { size : Int
        , color : Color
        , family : FontFamily
        , weight : FontWeight
        , onSizeChange : String -> Msg
        , onColorChange : String -> Msg
        , onFamilyChange : FontFamily -> Msg
        , onWeightChange : FontWeight -> Msg
        , sizeField : Field
        , colorField : Field
        }
    -> Html Msg
fontView model { size, color, family, weight, sizeField, onSizeChange, onFamilyChange, onWeightChange } =
    let
        size_ =
            case model.inspector of
                EditingField ThemeTextFontSizeField new ->
                    new

                _ ->
                    String.fromInt size
    in
    H.div []
        [ H.div [ A.class "form-group" ]
            [ fontFamilyView family onFamilyChange
            ]
        , H.div [ A.class "d-flex" ]
            [ H.div [ A.class "form-group mr-1 w-25" ]
                [ H.input
                    [ A.id (Common.fieldId sizeField)
                    , A.classList
                        [ ( "form-control form-control-sm text-center", True )
                        ]
                    , A.type_ "number"
                    , A.min (String.fromInt Font.minFontSizeAllowed)
                    , A.value size_
                    , E.onFocus (FieldEditingStarted sizeField size_)
                    , E.onBlur FieldEditingFinished
                    , E.onInput FieldChanged
                    ]
                    []
                    |> addDropdown FontSizeField model.dropDownState (fontSizeItems onSizeChange)
                ]
            , H.div [ A.class "form-group w-75" ]
                [ Common.fontWeightView family weight onWeightChange
                ]
            ]
        , Common.colorView model (Just color) FontColorField FontColorChanged
        ]


fontSizeItems msg =
    List.map
        (\value ->
            H.div [ E.onClick (msg value), A.class "dropdown-item text-center unselectable" ]
                [ H.text value ]
        )
        Common.fontSizes


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
    H.div [ A.class "form-group mb-2 row align-items-center" ]
        [ H.label [ A.class "col-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Spacing"
            ]
        , H.div [ A.class "col-9" ]
            [ H.div [ A.class "d-flex justify-content-end", A.style "gap" ".25rem" ]
                [ case node.type_ of
                    ParagraphNode _ ->
                        Common.numericFieldView SpacingYField "Line" lineSpacing

                    HeadingNode _ ->
                        Common.numericFieldView SpacingYField "Line" lineSpacing

                    _ ->
                        Common.none
                , Common.numericFieldView WordSpacingField "Word" wordSpacing
                , Common.numericFieldView LetterSpacingField "Letter" letterSpacing
                ]
            ]
        ]


fontFamilyView : FontFamily -> (FontFamily -> Msg) -> Html Msg
fontFamilyView fontFamily msg =
    let
        setSelected other attrs =
            A.selected (fontFamily.name == other) :: attrs
    in
    Keyed.node "select"
        [ onFontFamilySelect msg
        , A.classList
            [ ( "custom-select custom-select-sm", True )
            ]
        ]
        (Fonts.families
            |> List.map
                (\( group, families ) ->
                    ( group
                    , Keyed.node "optgroup"
                        [ A.attribute "label" group ]
                        (List.map
                            (\family ->
                                ( family.name
                                , H.option (setSelected family.name [ fontFamilyValue family ])
                                    [ H.text family.name ]
                                )
                            )
                            families
                        )
                    )
                )
        )


fontFamilyValue : FontFamily -> Attribute msg
fontFamilyValue value =
    A.value (Codecs.encodeFontFamily value)


onFontFamilySelect msg =
    E.on "input" (Codecs.fontFamilyDecoder msg)
