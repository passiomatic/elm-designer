module Views.Inspector exposing (view)

{- Inspector allows to edit node style information. -}

import Array
import Bootstrap.Tab as Tab
import Codecs
import Css exposing (em, percent, px)
import Dict exposing (Dict)
import Document exposing (..)
import Element exposing (Color, Orientation(..))
import Element.Background exposing (image)
import Fonts
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Entity as Entity
import Html.Events as E
import Html.Events.Extra.Wheel as Wheel
import Html.Keyed as Keyed
import Html5.DragDrop as DragDrop
import Icons exposing (arrowLeftAnim)
import Model exposing (..)
import Palette
import SelectList exposing (SelectList)
import Style.Background as Background exposing (Background)
import Style.Font as Font exposing (..)
import Style.Layout as Layout exposing (..)
import Style.Theme as Theme
import Svg.Attributes exposing (fontFamily)
import Tree as T exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import Views.Common exposing (fieldId, none)


view : Model -> List (Html Msg)
view model =
    let
        zipper =
            SelectList.selected model.pages
    in
    [ H.form [ E.onSubmit FieldEditingFinished ]
        -- This makes onSubmit to work when hitting enter
        (H.button [ A.type_ "submit", A.class "sr-only" ] []
            :: resolveStyleViews model zipper
        )
    ]


resolveStyleViews : Model -> Zipper Node -> List (Html Msg)
resolveStyleViews model zipper =
    let
        node =
            Zipper.label zipper

        title =
            H.div [ A.class "bpx-3 bpt-3 font-weight-500" ]
                [ H.text (Document.nodeType node.type_) ]
    in
    title
        :: (case node.type_ of
                PageNode ->
                    [ sectionView "Layout"
                        [ spacingYView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        ]
                    , bordersView model node
                    , backgroundView model node
                    ]

                RowNode data ->
                    [ sectionView ""
                        [ wrapRowOptionView data.wrapped
                        ]
                    , sectionView "Layout"
                        [ positionView model node
                        , lengthView model node
                        , spacingXView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        ]
                    , bordersView model node
                    , backgroundView model node
                    ]

                ColumnNode ->
                    [ sectionView "Layout"
                        [ positionView model node
                        , lengthView model node
                        , spacingYView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        ]
                    , bordersView model node
                    , backgroundView model node
                    ]

                TextColumnNode ->
                    [ sectionView "Text"
                        [ fontView model zipper
                        ]
                    , sectionView "Layout"
                        [ positionView model node
                        , lengthView model node
                        , spacingXView model node
                        , spacingYView model node
                        , paddingView model node
                        ]
                    , bordersView model node
                    , backgroundView model node
                    ]

                TextFieldNode label ->
                    [ sectionView ""
                        [ labelView label model node
                        , spacingYView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        , textAlignmentView model node.textAlignment
                        ]
                    , sectionView "Layout"
                        [ positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , bordersView model node
                    , backgroundView model node
                    ]

                TextFieldMultilineNode label ->
                    [ sectionView ""
                        [ labelView label model node
                        , spacingYView model node
                        ]
                    , sectionView "Layout"
                        [ positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        , textAlignmentView model node.textAlignment
                        ]
                    , bordersView model node
                    , backgroundView model node
                    ]

                CheckboxNode label ->
                    [ sectionView ""
                        [ labelView label model node
                        , spacingXView model node
                        ]
                    , sectionView "Layout"
                        [ positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        , textAlignmentView model node.textAlignment
                        ]
                    , bordersView model node
                    , backgroundView model node
                    ]

                ButtonNode button ->
                    [ sectionView ""
                        [ labelView button model node
                        ]
                    , sectionView "Layout"
                        [ positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        , textAlignmentView model node.textAlignment
                        ]
                    , bordersView model node
                    , backgroundView model node
                    ]

                RadioNode label ->
                    [ sectionView ""
                        [ labelView label model node
                        ]
                    , sectionView "Layout"
                        [ positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        ]
                    , bordersView model node
                    , backgroundView model node
                    ]

                OptionNode option ->
                    [ sectionView ""
                        [ labelView option model node
                        ]
                    , sectionView "Layout"
                        [ positionView model node
                        , lengthView model node
                        , paddingView model node
                        ]
                    , sectionView "Text"
                        [ fontView model zipper
                        ]
                    , bordersView model node
                    , backgroundView model node
                    ]

                _ ->
                    commonViews zipper model node
           )


commonViews zipper model node =
    [ sectionView "Layout"
        [ positionView model node
        , lengthView model node
        , paddingView model node
        ]
    , sectionView "Text"
        [ fontView model zipper
        , textAlignmentView model node.textAlignment
        ]
    , bordersView model node
    , backgroundView model node
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


labelView : { a | text : String } -> Model -> Node -> Html Msg
labelView { text } model { type_ } =
    let
        label_ =
            case model.inspector of
                EditingField LabelField _ new ->
                    new

                _ ->
                    text
    in
    H.div [ A.class "form-group row align-items-center mb-2" ]
        [ H.label [ A.class "col-sm-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Label" ]
        , H.div [ A.class "col-sm-9", A.attribute "role" "group" ]
            [ H.input
                [ A.id (fieldId LabelField)
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


imageView : ImageData -> Model -> Node -> Html Msg
imageView image model _ =
    let
        imageSrc =
            case model.inspector of
                EditingField ImageSrcField _ new ->
                    new

                _ ->
                    image.src
    in
    H.section [ A.class "section bp-3  border-bottom" ]
        [ H.h2 [ A.class "section__title mb-2" ]
            [ H.text "Image" ]
        , H.div [ A.class "" ]
            [ H.div [ A.class "form-group m-0" ]
                [ H.input
                    [ A.id (fieldId ImageSrcField)
                    , A.type_ "text"
                    , A.value imageSrc
                    , A.placeholder "https://domain.com/sample.jpg"
                    , A.class "form-control form-control-sm"
                    , E.onFocus (FieldEditingStarted ImageSrcField imageSrc)
                    , E.onBlur FieldEditingFinished
                    , E.onInput FieldChanged
                    ]
                    []
                , H.label [ A.for (fieldId ImageSrcField), A.class "small m-0" ]
                    [ H.text "Image address" ]
                ]
            ]
        ]


paddingView : Model -> Node -> Html Msg
paddingView model { padding } =
    let
        paddingTop =
            case model.inspector of
                EditingField PaddingTopField _ new ->
                    new

                EditingField PaddingRightField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.top

                EditingField PaddingBottomField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.top

                EditingField PaddingLeftField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.top

                _ ->
                    String.fromInt padding.top

        paddingRight =
            case model.inspector of
                EditingField PaddingRightField _ new ->
                    new

                EditingField PaddingTopField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.right

                EditingField PaddingBottomField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.right

                EditingField PaddingLeftField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.right

                _ ->
                    String.fromInt padding.right

        paddingBottom =
            case model.inspector of
                EditingField PaddingBottomField _ new ->
                    new

                EditingField PaddingTopField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.bottom

                EditingField PaddingRightField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.bottom

                EditingField PaddingLeftField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.bottom

                _ ->
                    String.fromInt padding.bottom

        paddingLeft =
            case model.inspector of
                EditingField PaddingLeftField _ new ->
                    new

                EditingField PaddingTopField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.left

                EditingField PaddingRightField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.left

                EditingField PaddingBottomField old new ->
                    if padding.locked && new /= old then
                        new

                    else
                        String.fromInt padding.left

                _ ->
                    String.fromInt padding.left
    in
    H.div [ A.class "form-group row align-items-center mb-0" ]
        [ H.label [ A.class "col-sm-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Padding" ]
        , H.div [ A.class "col-sm-9" ]
            [ H.div [ A.class "d-flex justify-content-center mb-1" ]
                [ H.div [ A.class "w-25" ]
                    [ H.input
                        [ A.id (fieldId PaddingTopField)
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
                        [ A.id (fieldId PaddingLeftField)
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
                        [ A.classList
                            [ ( "btn btn-link", True )
                            , ( "text-dark", not padding.locked )
                            ]
                        , E.onClick (PaddingLockChanged (not padding.locked))
                        , A.title "Use a single padding for all directions"
                        , A.type_ "button"
                        ]
                        [ if padding.locked then
                            Icons.lock

                          else
                            Icons.unlock
                        ]
                    ]
                , H.div [ A.class "w-25" ]
                    [ H.input
                        [ A.id (fieldId PaddingRightField)
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
                        [ A.id (fieldId PaddingBottomField)
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


spacingXView : Model -> Node -> Html Msg
spacingXView model { spacing } =
    let
        x =
            case spacing of
                Spacing ( x_, _ ) ->
                    case model.inspector of
                        EditingField SpacingXField _ new ->
                            new

                        _ ->
                            String.fromInt x_

                SpaceEvenly ->
                    "Evenly"
    in
    H.div [ A.class "form-group row align-items-center mb-2" ]
        [ H.label [ A.class "col-sm-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Spacing X" ]
        , H.div [ A.class "col-sm-9" ]
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
                        EditingField SpacingYField _ new ->
                            new

                        _ ->
                            String.fromInt y_

                SpaceEvenly ->
                    "Evenly"
    in
    H.div [ A.class "form-group row align-items-center mb-2" ]
        [ H.label [ A.class "col-sm-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Spacing Y" ]
        , H.div [ A.class "col-sm-9" ]
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
                    , ( fieldId fieldId_ ++ "-dropdown", True )
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
                EditingField BorderTopLeftCornerField _ new ->
                    new

                _ ->
                    String.fromInt borderCorner.topLeft

        topRightCorner =
            case model.inspector of
                EditingField BorderTopRightCornerField _ new ->
                    new

                _ ->
                    String.fromInt borderCorner.topRight

        bottomRightCorner =
            case model.inspector of
                EditingField BorderBottomRightCornerField _ new ->
                    new

                _ ->
                    String.fromInt borderCorner.bottomRight

        bottomLeftCorner =
            case model.inspector of
                EditingField BorderBottomLeftCornerField _ new ->
                    new

                _ ->
                    String.fromInt borderCorner.bottomLeft

        -- Widths
        topWidth =
            case model.inspector of
                EditingField BorderTopWidthField _ new ->
                    new

                _ ->
                    String.fromInt borderWidth.top

        rightWidth =
            case model.inspector of
                EditingField BorderRightWidthField _ new ->
                    new

                _ ->
                    String.fromInt borderWidth.right

        bottomWidth =
            case model.inspector of
                EditingField BorderBottomWidthField _ new ->
                    new

                _ ->
                    String.fromInt borderWidth.bottom

        leftWidth =
            case model.inspector of
                EditingField BorderLeftWidthField _ new ->
                    new

                _ ->
                    String.fromInt borderWidth.left
    in
    H.section [ A.class "section bp-3  border-bottom" ]
        [ H.h2 [ A.class "section__title mb-2" ]
            [ H.text "Border" ]
        , H.div [ A.class "form-group row align-items-center mb-2" ]
            [ H.label [ A.class "col-sm-3 col-form-label-sm m-0 text-nowrap" ]
                [ H.text "Size" ]
            , H.div [ A.class "col-sm-9" ]
                [ H.div [ A.class "d-flex justify-content-between mb-1" ]
                    [ H.div [ A.class "w-25 mr-1" ]
                        [ H.div [ A.class "input-group input-group-sm" ]
                            [ H.div [ A.class "input-group-prepend" ]
                                [ H.span [ A.class "input-group-text bpx-1" ] [ H.text Entity.ulcorner ]
                                ]
                            , H.input
                                [ A.id (fieldId BorderTopLeftCornerField)
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
                            [ A.id (fieldId BorderTopWidthField)
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
                                [ A.id (fieldId BorderTopRightCornerField)
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
                            [ A.id (fieldId BorderLeftWidthField)
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
                            [ A.id (fieldId BorderRightWidthField)
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
                                [ A.id (fieldId BorderBottomLeftCornerField)
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
                            [ A.id (fieldId BorderBottomWidthField)
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
                                [ A.id (fieldId BorderBottomRightCornerField)
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
        , colorView model (Just borderColor) BorderColorField BorderColorChanged
        ]


colorView : Model -> Maybe Color -> Field -> (String -> Msg) -> Html Msg
colorView model color field msg =
    H.div [ A.class "form-group row align-items-center mb-2" ]
        [ H.label [ A.class "col-sm-3 col-form-label-sm m-0" ]
            [ H.text "Color" ]
        , H.div [ A.class "col-sm-9 d-flex" ]
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
                EditingField field_ _ new ->
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


backgroundView : Model -> Node -> Html Msg
backgroundView model { backgroundColor, background } =
    let
        image_ =
            case model.inspector of
                EditingField BackgroundImageField _ new ->
                    new

                _ ->
                    backgroundImageUrl background
    in
    H.section [ A.class "section bp-3 border-bottom" ]
        [ H.h2 [ A.class "section__title mb-2" ]
            [ H.text "Background" ]
        , colorView model backgroundColor BackgroundColorField BackgroundColorChanged
        , H.div [ A.class "form-group row align-items-center mb-2" ]
            [ H.label [ A.for (fieldId BackgroundImageField), A.class "col-sm-3 col-form-label-sm m-0 text-nowrap" ]
                [ H.text "Image URL" ]
            , H.div [ A.class "col-sm-9" ]
                [ H.input
                    [ A.id (fieldId BackgroundImageField)
                    , A.type_ "text"
                    , A.value image_
                    , A.placeholder ""
                    , A.autocomplete False
                    , A.class "form-control form-control-sm"
                    , E.onFocus (FieldEditingStarted BackgroundImageField image_)
                    , E.onBlur FieldEditingFinished
                    , E.onInput FieldChanged
                    ]
                    []
                ]
            ]
        , backgroundSizingView model background
        ]


backgroundSizingView : Model -> Background -> Html Msg
backgroundSizingView model value =
    if value == Background.None then
        none

    else
        let
            url =
                backgroundImageUrl value
        in
        H.div [ A.class "form-group row align-items-center mb-2" ]
            [ H.label [ A.class "col-sm-3 col-form-label-sm m-0 text-nowrap" ]
                [ H.text "Sizing" ]
            , H.div [ A.class "col-sm-9 btn-group", A.attribute "role" "group" ]
                [ H.button
                    [ A.classList
                        [ ( "btn btn-light btn-sm w-33", True )
                        , ( "active", isCropped value )
                        ]
                    , A.type_ "button"
                    , A.title "Fit the containing element by cropping the image"
                    , E.onClick (BackgroundSizingChanged (Background.Cropped url))
                    ]
                    [ H.text "Crop" ]
                , H.button
                    [ A.classList
                        [ ( "btn btn-light btn-sm w-33", True )
                        , ( "active", isUncropped value )
                        ]
                    , A.type_ "button"
                    , A.title "Fit the containing element by scaling the image"
                    , E.onClick (BackgroundSizingChanged (Background.Uncropped url))
                    ]
                    [ H.text "Uncrop" ]
                , H.button
                    [ A.classList
                        [ ( "btn btn-light btn-sm w-33", True )
                        , ( "active", isTiled value )
                        ]
                    , A.type_ "button"
                    , A.title "Tile the image along the X and Y axes"
                    , E.onClick (BackgroundSizingChanged (Background.Tiled url))
                    ]
                    [ H.text "Tile" ]
                ]
            ]


backgroundImageUrl : Background -> String
backgroundImageUrl value =
    case value of
        Background.Cropped value_ ->
            value_

        Background.Uncropped value_ ->
            value_

        Background.Tiled value_ ->
            value_

        Background.None ->
            ""


isCropped : Background -> Bool
isCropped value =
    case value of
        Background.Cropped value_ ->
            True

        _ ->
            False


isUncropped value =
    case value of
        Background.Uncropped value_ ->
            True

        _ ->
            False


isTiled value =
    case value of
        Background.Tiled value_ ->
            True

        _ ->
            False


lengthView : Model -> Node -> Html Msg
lengthView model node =
    H.div [ A.class "mb-3" ]
        [ widthView model node
        , heightView model node
        ]


wrapRowOptionView : Bool -> Html Msg
wrapRowOptionView wrapped =
    H.div [ A.class "custom-control custom-checkbox mb-0" ]
        [ H.input
            [ A.class "custom-control-input"
            , A.id "wrap-row-items"
            , A.type_ "checkbox"
            , A.checked wrapped
            , E.onCheck WrapRowItemsChanged
            ]
            []
        , H.label [ A.class "custom-control-label", A.for "wrap-row-items" ]
            [ H.text "Wrap items" ]
        ]


widthView : Model -> Node -> Html Msg
widthView _ { width } =
    H.div [ A.class "form-group row align-items-center mb-2" ]
        [ H.label [ A.class "col-sm-3 col-form-label-sm m-0" ]
            [ H.text "Width" ]
        , H.div [ A.class "col-sm-9 btn-group", A.attribute "role" "group" ]
            [ H.button
                [ A.classList
                    [ ( "btn btn-light btn-sm w-33", True )
                    , ( "active", width == Auto )
                    ]
                , E.onClick (WidthChanged Auto)
                , A.type_ "button"
                ]
                [ H.text "Auto" ]
            , H.button
                [ A.classList
                    [ ( "btn btn-light btn-sm w-33", True )
                    , ( "active", width == Fill )
                    ]
                , E.onClick (WidthChanged Fill)
                , A.type_ "button"
                ]
                [ H.text "Fill" ]
            , H.button
                [ A.classList
                    [ ( "btn btn-light btn-sm w-33", True )
                    , ( "active", width == Shrink )
                    ]
                , E.onClick (WidthChanged Shrink)
                , A.type_ "button"
                ]
                [ H.text "Shrink" ]
            ]
        ]


heightView : Model -> Node -> Html Msg
heightView _ { height } =
    H.div [ A.class "form-group row align-items-center mb-2" ]
        [ H.label [ A.class "col-sm-3 col-form-label-sm m-0" ]
            [ H.text "Height" ]
        , H.div [ A.class "col-sm-9 btn-group", A.attribute "role" "group" ]
            [ H.button
                [ A.classList
                    [ ( "btn btn-light btn-sm w-33", True )
                    , ( "active", height == Auto )
                    ]
                , E.onClick (HeightChanged Auto)
                , A.type_ "button"
                ]
                [ H.text "Auto" ]
            , H.button
                [ A.classList
                    [ ( "btn btn-light btn-sm w-33", True )
                    , ( "active", height == Fill )
                    ]
                , E.onClick (HeightChanged Fill)
                , A.type_ "button"
                ]
                [ H.text "Fill" ]
            , H.button
                [ A.classList
                    [ ( "btn btn-light btn-sm w-33", True )
                    , ( "active", height == Shrink )
                    ]
                , E.onClick (HeightChanged Shrink)
                , A.type_ "button"
                ]
                [ H.text "Shrink" ]
            ]
        ]


positionView : Model -> Node -> Html Msg
positionView model ({ transformation } as node) =
    let
        offsetX =
            case model.inspector of
                EditingField OffsetXField _ new ->
                    new

                _ ->
                    String.fromFloat transformation.offsetX

        offsetY =
            case model.inspector of
                EditingField OffsetYField _ new ->
                    new

                _ ->
                    String.fromFloat transformation.offsetY
    in
    H.div [ A.class "form-group row align-items-center mb-3" ]
        [ H.label [ A.class "col-sm-3 col-form-label-sm" ]
            [ H.text "Position" ]
        , H.div [ A.class "col-sm-9" ]
            [ H.div [ A.class "d-flex align-items-center mb-1" ]
                [ alignmentView model node
                , H.div [ A.class "w-33 ml-1" ]
                    [ H.input
                        [ A.id (fieldId OffsetXField)
                        , A.class "form-control form-control-sm text-center mx-auto"
                        , A.type_ "number"
                        , A.value offsetX
                        , A.title "Move right/left"
                        , E.onFocus (FieldEditingStarted OffsetXField offsetX)
                        , E.onBlur FieldEditingFinished
                        , E.onInput FieldChanged
                        ]
                        []
                    ]
                ]
            , H.div [ A.class "mr-1" ]
                [ H.input
                    [ A.id (fieldId OffsetYField)
                    , A.class "form-control form-control-sm text-center mx-auto w-33"
                    , A.type_ "number"
                    , A.value offsetY
                    , A.title "Move bottom/top"
                    , E.onFocus (FieldEditingStarted OffsetYField offsetY)
                    , E.onBlur FieldEditingFinished
                    , E.onInput FieldChanged
                    ]
                    []
                ]
            ]
        ]


alignmentView : Model -> Node -> Html Msg
alignmentView _ { alignmentX, alignmentY } =
    let
        nextAlignLeft =
            nextAlignStartState alignmentX

        nextAlignRight =
            nextAlignEndState alignmentX

        nextAlignTop =
            nextAlignStartState alignmentY

        nextAlignBottom =
            nextAlignEndState alignmentY
    in
    H.div [ A.class "bg-white border rounded ml-auto w-33" ]
        -- Top align
        [ H.div [ A.class "d-flex justify-content-center" ]
            [ H.button
                [ A.classList
                    [ ( "bp-0 border-0 bg-white line-height-1 text-black-25", True )
                    , ( "text-primary", alignmentY == Start || alignmentY == Center )
                    ]
                , E.onClick (AlignmentYChanged nextAlignTop)
                , A.title "Align top"
                ]
                [ Icons.pipe ]
            ]
        , H.div [ A.class "d-flex align-items-center justify-content-between" ]
            -- Left align
            [ H.button
                [ A.classList
                    [ ( "rotate-90 bp-0 border-0 bg-white line-height-1 text-black-25", True )
                    , ( "text-primary", alignmentX == Start || alignmentX == Center )
                    ]
                , E.onClick (AlignmentXChanged nextAlignLeft)
                , A.title "Align left"
                ]
                [ Icons.pipe ]
            , H.div [ A.class "bg-light border rounded", A.style "width" "1.5rem", A.style "height" "1.5rem" ] []

            -- Right align
            , H.button
                [ A.classList
                    [ ( "rotate-90 bp-0 border-0 bg-white line-height-1 text-black-25", True )
                    , ( "text-primary", alignmentX == End || alignmentX == Center )
                    ]
                , E.onClick (AlignmentXChanged nextAlignRight)
                , A.title "Align right"
                ]
                [ Icons.pipe ]
            ]

        -- Bottom align
        , H.div [ A.class "d-flex justify-content-center" ]
            [ H.button
                [ A.classList
                    [ ( "bp-0 border-0 bg-white line-height-1 text-black-25", True )
                    , ( "text-primary", alignmentY == End || alignmentY == Center )
                    ]
                , E.onClick (AlignmentYChanged nextAlignBottom)
                , A.title "Align bottom"
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
    H.div [ A.class "form-group row align-items-center mb-0" ]
        [ H.label [ A.class "col-sm-3 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Alignment" ]
        , H.div [ A.class "col-sm-9 btn-group", A.attribute "role" "group" ]
            [ H.button
                [ A.classList
                    [ ( "btn btn-light btn-sm w-25", True )
                    , ( "active", value == TextLeft )
                    ]
                , E.onClick (TextAlignChanged TextLeft)
                , A.type_ "button"
                ]
                [ Icons.alignLeft ]
            , H.button
                [ A.classList
                    [ ( "btn btn-light btn-sm w-25", True )
                    , ( "active", value == TextCenter )
                    ]
                , E.onClick (TextAlignChanged TextCenter)
                , A.type_ "button"
                ]
                [ Icons.alignCenter ]
            , H.button
                [ A.classList
                    [ ( "btn btn-light btn-sm w-25", True )
                    , ( "active", value == TextRight )
                    ]
                , E.onClick (TextAlignChanged TextRight)
                , A.type_ "button"
                ]
                [ Icons.alignRight ]
            , H.button
                [ A.classList
                    [ ( "btn btn-light btn-sm w-25", True )
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

        fontSize_ =
            case model.inspector of
                EditingField FontSizeField _ new ->
                    new

                _ ->
                    case node.fontSize of
                        Local value ->
                            String.fromInt value

                        Inherit ->
                            "("
                                ++ (Document.resolveInheritedFontSize theme.textSize zipper
                                        |> String.fromInt
                                   )
                                ++ ")"

        resolvedFontFamily =
            Document.resolveInheritedFontFamily theme.textFontFamily zipper

        fontColor_ =
            case node.fontColor of
                Local value ->
                    value

                Inherit ->
                    Document.resolveInheritedFontColor theme.textColor zipper
    in
    H.div []
        [ H.div [ A.class "form-group" ]
            [ fontFamilyView node.fontFamily resolvedFontFamily (canInherit node)
            ]
        , H.div [ A.class "d-flex" ]
            [ H.div [ A.class "form-group mr-2 w-25" ]
                [ H.input
                    [ A.id (fieldId FontSizeField)
                    , A.class "form-control form-control-sm"
                    , A.type_ "number"
                    , A.min (String.fromInt Font.minFontSizeAllowed)
                    , A.value fontSize_
                    , E.onFocus (FieldEditingStarted FontSizeField fontSize_)
                    , E.onBlur FieldEditingFinished
                    , E.onInput FieldChanged
                    ]
                    []
                    |> addDropdown FontSizeField model.dropDownState (fontSizeItems node)
                ]
            , H.div [ A.class "form-group w-75" ]
                [ fontWeightView resolvedFontFamily node.fontWeight
                ]
            ]
        , colorView model (Just fontColor_) FontColorField FontColorChanged
        , case node.type_ of
            ParagraphNode _ ->
                liheHeightView model node.spacing

            HeadingNode _ ->
                liheHeightView model node.spacing

            _ ->
                none
        ]


fontView_ : Zipper Node -> Model -> Node -> Html Msg
fontView_ zipper model ({ fontSize, fontWeight, fontFamily, fontColor, textAlignment, spacing, type_ } as node) =
    let
        theme =
            Theme.defaultTheme

        fontSize_ =
            case model.inspector of
                EditingField FontSizeField _ new ->
                    new

                _ ->
                    case fontSize of
                        Local value ->
                            String.fromInt value

                        Inherit ->
                            "("
                                ++ (Document.resolveInheritedFontSize theme.textSize zipper
                                        |> String.fromInt
                                   )
                                ++ ")"

        resolvedFontFamily =
            Document.resolveInheritedFontFamily theme.textFontFamily zipper

        fontColor_ =
            case fontColor of
                Local value ->
                    value

                Inherit ->
                    Document.resolveInheritedFontColor theme.textColor zipper
    in
    H.section [ A.class "section bp-3 border-bottom" ]
        [ H.h2 [ A.class "section__title" ]
            [ H.text "Text" ]
        , H.div [ A.class "form-group" ]
            [ fontFamilyView fontFamily resolvedFontFamily (canInherit node)
            ]
        , H.div [ A.class "d-flex" ]
            [ H.div [ A.class "form-group mr-2 w-25" ]
                [ H.input
                    [ A.id (fieldId FontSizeField)
                    , A.class "form-control form-control-sm"
                    , A.type_ "text"
                    , A.value fontSize_
                    , E.onFocus (FieldEditingStarted FontSizeField fontSize_)
                    , E.onBlur FieldEditingFinished
                    , E.onInput FieldChanged
                    ]
                    []
                    |> addDropdown FontSizeField model.dropDownState (fontSizeItems node)
                ]
            , H.div [ A.class "form-group w-75" ]
                [ fontWeightView resolvedFontFamily fontWeight
                ]
            ]
        , colorView model (Just fontColor_) FontColorField FontColorChanged
        , case node.type_ of
            ParagraphNode _ ->
                liheHeightView model spacing

            HeadingNode _ ->
                liheHeightView model spacing

            _ ->
                none
        , textAlignmentView model textAlignment
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


liheHeightView : Model -> Spacing -> Html Msg
liheHeightView model spacing =
    let
        spacing_ =
            case spacing of
                Spacing ( _, y ) ->
                    case model.inspector of
                        EditingField SpacingYField _ new ->
                            new

                        _ ->
                            String.fromInt y

                SpaceEvenly ->
                    -- TODO figure out if it makes sense here
                    ""
    in
    H.div [ A.class "form-group row align-items-center mb-2" ]
        [ H.label [ A.class "col-sm-6 col-form-label-sm m-0 text-nowrap" ]
            [ H.text "Line spacing" ]
        , H.div [ A.class "col-sm-6 btn-group", A.attribute "role" "group" ]
            [ H.div [ A.class "form-group m-0" ]
                [ H.input
                    [ A.class "form-control form-control-sm"
                    , A.type_ "number"
                    , A.min "0"
                    , A.value spacing_
                    , E.onFocus (FieldEditingStarted SpacingYField spacing_)
                    , E.onBlur FieldEditingFinished
                    , E.onInput FieldChanged
                    ]
                    []
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

        inheritedLabel name =
            "(" ++ name ++ ")"

        inheritOption =
            case fontFamily of
                Local _ ->
                    if inherit then
                        ( "inherit"
                        , H.option [ fontFamilyValue Inherit ]
                            [ H.text "Inherit" ]
                        )

                    else
                        ( "", none )

                Inherit ->
                    ( inheritedLabel resolvedFontFamily.name
                    , H.option [ A.disabled True, A.selected True ]
                        [ H.text (inheritedLabel resolvedFontFamily.name) ]
                    )
    in
    Keyed.node "select"
        [ onFontFamilySelect FontFamilyChanged, A.class "custom-select custom-select-sm" ]
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
        [ onFontWeightSelect FontWeightChanged, A.class "custom-select custom-select-sm" ]
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
