module Views.Common exposing
    ( canDropInto
    , canDropSibling
    , colorView
    , fieldId
    , fontSizes
    , fontWeightView
    , isDragging
    , none
    , numericFieldView
    )

import Codecs
import Css
import Document exposing (DragId(..))
import Element exposing (Color)
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Html5.DragDrop as DragDrop
import Model exposing (Field(..), Inspector(..), Model, Msg(..))
import Palette
import Style.Font as Font exposing (FontFamily, FontWeight(..))
import Tree as T exposing (Tree)


{-| Classic typographic scale.

    See https://www.kevinpowell.co/article/typographic-scale/

-}
fontSizes =
    [ 11, 12, 14, 16, 18, 24, 30, 36, 48, 60, 72 ]
        |> List.map String.fromInt


fontWeightView : FontFamily -> FontWeight -> (FontWeight -> Msg) -> Html Msg
fontWeightView fontFamily fontWeight msg =
    let
        setSelected other attrs =
            A.selected (fontWeight == other) :: attrs
    in
    Keyed.node "select"
        [ onFontWeightSelect msg, A.class "custom-select custom-select-sm" ]
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


numericFieldView : Field -> String -> String -> Html Msg
numericFieldView field label value =
    H.div [ A.class "w-33" ]
        [ H.label [ A.class "col-form-label-sm m-0 p-0", A.for (fieldId field) ]
            [ H.text label
            ]
        , H.input
            [ A.id (fieldId field)
            , A.class "form-control form-control-sm text-center"
            , A.type_ "number"

            --, A.min "0"
            , A.value value
            , E.onFocus (FieldEditingStarted field value)
            , E.onBlur FieldEditingFinished
            , E.onInput FieldChanged
            ]
            []
        ]


colorView : Model -> Maybe Color -> Field -> (String -> Msg) -> Html Msg
colorView model color field msg =
    H.div [ A.class "form-group row align-items-center mb-2" ]
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
            [ ( "form-control form-control-sm mr-1", True )
            , ( "transparent", value == Nothing )
            ]
        ]
        []


colorHexView : Model -> Maybe Color -> Field -> Html Msg
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
        [ H.div [ A.class "input-group-prepend" ]
            [ H.span [ A.class "input-group-text bpx-1" ] [ H.text "#" ]
            ]
        , H.input
            [ A.id (fieldId field)
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
--     H.div [ A.class "form-group m-0 w-33" ]
--         [ H.label [ A.for (fieldId BorderColorField), A.class "small m-0" ]
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
--             , H.div [ A.class "input-group-append" ] [ H.span [ A.class "input-group-text bpx-1" ] [ H.text "%" ] ]
--             ]
--         -- , H.label [ A.for name, A.class "small m-0" ]
--         --     [ H.text "Opacity" ]
--         ]


isDragging dragDrop =
    DragDrop.getDragId dragDrop /= Nothing


canDropInto container dragDrop =
    case DragDrop.getDragId dragDrop of
        Just dragId ->
            case dragId of
                Move node ->
                    Document.canDropInto container node

                Insert template ->
                    Document.canDropInto container (T.label template)

        Nothing ->
            False


canDropSibling sibling dragDrop =
    case DragDrop.getDragId dragDrop of
        Just dragId ->
            case dragId of
                Move node ->
                    Document.canDropSibling sibling node

                Insert template ->
                    Document.canDropSibling sibling (T.label template)

        Nothing ->
            False


fieldId : Field -> String
fieldId field =
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

        ThemeTextFontSizeField ->
            "theme-text-font-size"

        ThemeTextColorField ->
            "theme-text-color"

       

none =
    H.div [] []
