module Views.ElmUI exposing (render)

{- Elm UI renderer for the page node element. -}

import Debug
import Document exposing (..)
import Element as E exposing (Color, Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (Option)
import Element.Region as Region
import File exposing (File)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events
import Html5.DragDrop as DragDrop
import Icons
import Json.Decode as Decode exposing (Decoder)
import Model exposing (..)
import Palette
import SelectList exposing (SelectList)
import Style.Background as Background exposing (Background)
import Style.Border exposing (BorderCorner, BorderStyle(..), BorderWidth)
import Style.Font as Font exposing (..)
import Style.Layout as Layout exposing (..)
import Tree as T exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import Views.Common as Common


type RenderedNode
    = RenderedElement (Element Msg)
    | RenderedOption (Option NodeId Msg)


render : Context -> Tree Node -> Html Msg
render ctx tree =
    let
        rootElement node =
            case node of
                RenderedElement el ->
                    el

                RenderedOption _ ->
                    -- We don't have any radio option at top level
                    E.none
    in
    T.restructure identity (renderNode ctx) tree
        |> rootElement
        |> (case ( ctx.mode, ctx.inspector ) of
                ( DesignMode, NotEdited ) ->
                    -- Avoid focus rings in design mode while not editing any text/fields
                    E.layoutWith
                        { options = [ E.focusStyle noFocusStyle ]
                        }
                        []

                ( _, _ ) ->
                    E.layout []
           )


noFocusStyle =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


renderNode : Context -> Node -> List RenderedNode -> RenderedNode
renderNode ctx node children =
    let
        selected =
            Document.isSelected node.id ctx.currentNode
    in
    case node.type_ of
        PageNode ->
            renderPage ctx node selected children

        ParagraphNode data ->
            renderParagraph ctx node selected data

        TextNode data ->
            renderText ctx node selected data

        ImageNode data ->
            renderImage ctx node selected data

        HeadingNode data ->
            renderHeading ctx node selected data

        ColumnNode ->
            renderColumn ctx node selected children

        TextColumnNode ->
            renderTextColumn ctx node selected children

        RowNode data ->
            renderRow ctx node selected data children

        TextFieldNode data ->
            renderTextField ctx node selected data

        TextFieldMultilineNode data ->
            renderTextFieldMultiline ctx node selected data

        ButtonNode data ->
            renderButton ctx node selected data

        CheckboxNode data ->
            renderCheckbox ctx node selected data

        RadioNode data ->
            renderRadio ctx node selected data children

        OptionNode data ->
            renderOption ctx node selected data



-- ELEMENTS


renderTextColumn : Context -> Node -> Bool -> List RenderedNode -> RenderedNode
renderTextColumn ctx node selected children =
    let
        newChildren =
            if List.length children == 0 then
                [ placeholderText "Empty Text Column" ]

            else
                elements children

        attrs =
            [ elementClasses ctx node selected
            , elementId node
            , onClick (NodeSelected node.id)
            ]
                |> makeDroppableIf (Common.canDropInto node ctx.dragDrop) (AppendTo node.id)
                |> makeFileDroppableIf (not <| Common.isDragging ctx.dragDrop) node.id
                |> applyAllStyles node
    in
    E.textColumn attrs newChildren
        |> RenderedElement


renderColumn : Context -> Node -> Bool -> List RenderedNode -> RenderedNode
renderColumn ctx node selected children =
    let
        newChildren =
            if List.length children == 0 then
                [ placeholderText "Empty Column" ]

            else
                elements children

        attrs =
            [ elementClasses ctx node selected
            , elementId node
            , onClick (NodeSelected node.id)
            ]
                |> makeDroppableIf (Common.canDropInto node ctx.dragDrop) (AppendTo node.id)
                |> makeFileDroppableIf (not <| Common.isDragging ctx.dragDrop) node.id
                |> applyAllStyles node
    in
    E.column attrs newChildren
        |> RenderedElement


renderRow : Context -> Node -> Bool -> RowData -> List RenderedNode -> RenderedNode
renderRow ctx node selected { wrapped } children =
    let
        newChildren =
            if List.length children == 0 then
                [ placeholderText "Empty Row" ]

            else
                elements children

        attrs =
            [ elementClasses ctx node selected
            , elementId node
            , onClick (NodeSelected node.id)
            ]
                |> makeDroppableIf (Common.canDropInto node ctx.dragDrop) (AppendTo node.id)
                |> makeFileDroppableIf (not <| Common.isDragging ctx.dragDrop) node.id
                |> applyAllStyles node
    in
    (if wrapped then
        E.wrappedRow attrs newChildren

     else
        E.row attrs newChildren
    )
        |> RenderedElement


{-| Render page as Elm UI column to layout elements vertically one after another, just like a regular HTML page.
-}
renderPage : Context -> Node -> Bool -> List RenderedNode -> RenderedNode
renderPage ctx node selected children =
    let
        attrs =
            [ elementClasses ctx node selected
            , elementId node
            , onClick (NodeSelected node.id)
            ]
                |> makeDroppableIf (Common.canDropInto node ctx.dragDrop) (AppendTo node.id)
                |> makeFileDroppableIf (not <| Common.isDragging ctx.dragDrop) node.id
                |> applyAllStyles node
    in
    RenderedElement
        (if List.isEmpty children then
            renderEmptyPage attrs

         else
            E.column attrs (elements children)
        )


renderEmptyPage attrs =
    E.column attrs
        [ E.el
            [ Font.size 18
            , Font.color Palette.lightCharcoal
            , Font.bold
            , E.centerX
            , E.centerY
            , E.moveUp 8
            ]
            (E.text "Page is empty")
        , E.el
            [ Font.size 14
            , Font.color Palette.lightCharcoal
            , E.centerX
            , E.centerY
            ]
            (E.text "Drop library elements here.")
        ]


renderImage : Context -> Node -> Bool -> ImageData -> RenderedNode
renderImage ctx node selected image =
    let
        attrs =
            [ elementClasses ctx node selected
            , elementId node
            , onClick (NodeSelected node.id)
            ]
                |> applyAllStyles node
    in
    E.image attrs image
        |> RenderedElement


renderParagraph : Context -> Node -> Bool -> TextData -> RenderedNode
renderParagraph ctx node selected { text } =
    renderParagraphHelper ctx node selected text


renderHeading : Context -> Node -> Bool -> HeadingData -> RenderedNode
renderHeading ctx node selected heading =
    -- Use a paragraph here since it allows to wrap text and to set a line height
    renderParagraphHelper ctx node selected heading.text


renderParagraphHelper : Context -> Node -> Bool -> String -> RenderedNode
renderParagraphHelper ctx node selected text =
    let
        attrs =
            [ elementClasses ctx node selected
            , elementId node
            ]
                |> applyAllStyles node
    in
    case ( ctx.inspector, selected ) of
        ( EditingText, True ) ->
            -- While editing capture the click events but do nothing so the event won't bubble up
            textEditor
                (onDoubleClick NoOp
                    :: onClick NoOp
                    :: attrs
                    |> forceBackgroundColor node.backgroundColor
                )
                text
                |> RenderedElement

        _ ->
            E.paragraph
                (onDoubleClick (TextEditingStarted node.id)
                    :: onClick (NodeSelected node.id)
                    :: attrs
                )
                (if String.trim text == "" then
                    [ E.text "Double click to edit text" ]

                 else
                    renderLines text
                )
                |> RenderedElement


renderLines : String -> List (Element msg)
renderLines text =
    String.lines text
        |> List.map E.text
        |> List.intersperse break


break =
    E.html (H.br [] [])


renderText : Context -> Node -> Bool -> TextData -> RenderedNode
renderText ctx node selected { text } =
    let
        attrs =
            [ elementClasses ctx node selected
            , elementId node
            ]
                |> applyAllStyles node
    in
    case ( ctx.inspector, selected ) of
        ( EditingText, True ) ->
            -- While editing capture the click events but do nothing so the event won't bubble up
            textEditor
                (onDoubleClick NoOp
                    :: onClick NoOp
                    :: attrs
                    |> forceBackgroundColor node.backgroundColor
                )
                text
                |> RenderedElement

        _ ->
            E.el
                (onDoubleClick (TextEditingStarted node.id)
                    :: onClick (NodeSelected node.id)
                    :: attrs
                )
                (if String.trim text == "" then
                    E.text "Double click to edit text"

                 else
                    E.text text
                )
                |> RenderedElement


renderTextField : Context -> Node -> Bool -> LabelData -> RenderedNode
renderTextField ctx node selected label =
    let
        attrs =
            -- Deactivate field while in design mode
            [ E.htmlAttribute (A.readonly (ctx.mode == DesignMode))
            , E.width E.fill
            ]
                |> applyChildStyles node
    in
    E.el
        ([ elementClasses ctx node selected
         , elementId node
         , onClick (NodeSelected node.id)
         ]
            |> applyWidth node.width
            |> applyHeight node.height
            |> applyAlignX node.alignmentX
            |> applyAlignY node.alignmentY
        )
        (Input.text
            attrs
            { onChange = \s -> NoOp
            , text = ""
            , placeholder = Nothing

            -- TODO Espose label.position in the inspector
            , label = Input.labelAbove [ Font.color ctx.theme.labelColor ] (E.text label.text)
            }
        )
        |> RenderedElement


renderTextFieldMultiline : Context -> Node -> Bool -> LabelData -> RenderedNode
renderTextFieldMultiline ctx node selected label =
    let
        attrs =
            -- Deactivate field while in design mode
            [ E.htmlAttribute (A.readonly (ctx.mode == DesignMode))
            , E.width E.fill
            ]
                |> applyChildStyles node
    in
    E.el
        ([ elementClasses ctx node selected
         , elementId node
         , onClick (NodeSelected node.id)
         ]
            |> applyWidth node.width
            |> applyHeight node.height
            |> applyAlignX node.alignmentX
            |> applyAlignY node.alignmentY
        )
        (Input.multiline
            attrs
            { onChange = \s -> NoOp
            , text = ""
            , placeholder = Nothing
            , spellcheck = False

            -- TODO Espose label.position in the inspector
            , label = Input.labelAbove [ Font.color ctx.theme.labelColor ] (E.text label.text)
            }
        )
        |> RenderedElement



--renderLabel : Context -> Node -> Label ->  List (Element Msg) -> Label Msg
-- renderLabel ctx node label =
--     let
--         selected =
--             Document.isSelected node.id ctx.currentNode
--         classes =
--             A.classList
--                 [ ( "element", True )
--                 , ( "element--selected", selected )
--                 ]
--         attrs =
--             [ onClick (NodeSelected node.id)
--             --, E.htmlAttribute (A.readonly True)
--             , E.htmlAttribute classes
--             , elementId node
--             ]
--                 |> applyStandardStyles node
--     in
--     -- TODO Honor label.position
--     Input.labelAbove attrs (E.text label.text)


renderButton : Context -> Node -> Bool -> TextData -> RenderedNode
renderButton ctx node selected { text } =
    let
        attrs =
            [ E.width E.fill
            ]
                |> applyChildStyles node
    in
    E.el
        ([ elementClasses ctx node selected
         , elementId node
         , onClick (NodeSelected node.id)
         ]
            |> applyWidth node.width
            |> applyHeight node.height
            |> applyAlignX node.alignmentX
            |> applyAlignY node.alignmentY
        )
        (Input.button
            attrs
            { onPress = Nothing
            , label = E.text text
            }
        )
        |> RenderedElement


renderCheckbox : Context -> Node -> Bool -> LabelData -> RenderedNode
renderCheckbox ctx node selected label =
    let
        attrs =
            [ E.width E.fill
            ]
                |> applyChildStyles node
    in
    E.el
        ([ elementClasses ctx node selected
         , elementId node
         , onClick (NodeSelected node.id)
         ]
            |> applyWidth node.width
            |> applyHeight node.height
            |> applyAlignX node.alignmentX
            |> applyAlignY node.alignmentY
        )
        (Input.checkbox
            attrs
            { onChange = \s -> NoOp
            , icon = Input.defaultCheckbox
            , checked = True
            , label =
                Input.labelRight
                    [ Font.color ctx.theme.labelColor ]
                    (E.text label.text)
            }
        )
        |> RenderedElement


renderRadio : Context -> Node -> Bool -> LabelData -> List RenderedNode -> RenderedNode
renderRadio ctx node selected label children =
    let
        attrs =
            [ E.width E.fill
            ]
                |> applyChildStyles node
    in
    E.el
        ([ elementClasses ctx node selected
         , elementId node
         , onClick (NodeSelected node.id)
         ]
            |> makeDroppableIf (Common.canDropInto node ctx.dragDrop) (AppendTo node.id)
            |> applyWidth node.width
            |> applyHeight node.height
            |> applyAlignX node.alignmentX
            |> applyAlignY node.alignmentY
        )
        (Input.radio
            attrs
            { onChange = \s -> NoOp
            , selected = Nothing
            , label =
                Input.labelAbove
                    [ Font.color ctx.theme.labelColor
                    ]
                    (E.text label.text)
            , options =
                options children
            }
        )
        |> RenderedElement


renderOption : Context -> Node -> Bool -> TextData -> RenderedNode
renderOption ctx node selected { text } =
    let
        attrs =
            [ elementClasses ctx node selected
            , elementId node
            , onClick (NodeSelected node.id)
            ]
                |> applyAllStyles node
    in
    Input.option node.id (E.el attrs (E.text text))
        |> RenderedOption



-- STYLES


applyAllStyles : Node -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyAllStyles node attrs =
    attrs
        |> applyBorderCorner node.borderCorner
        |> applyBorderWidth node.borderWidth
        |> applyBorderColor node.borderColor
        |> applyPadding node.padding
        |> applyWidth node.width
        |> applyHeight node.height
        |> applySpacing node.spacing
        |> applyTransformation node.transformation
        |> applyFontSize node.fontSize
        |> applyFontFamily node.fontFamily
        |> applyFontColor node.fontColor
        |> applyFontWeight node.fontWeight
        |> applyLetterSpacing node.letterSpacing
        |> applyWordSpacing node.wordSpacing
        |> applyTextAlign node.textAlignment
        |> applyAlignX node.alignmentX
        |> applyAlignY node.alignmentY
        |> applyBackground node.background
        |> applyBackgroundColor node.backgroundColor


applyChildStyles : Node -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyChildStyles node attrs =
    attrs
        |> applyBorderCorner node.borderCorner
        |> applyBorderWidth node.borderWidth
        |> applyBorderColor node.borderColor
        |> applyPadding node.padding
        |> applySpacing node.spacing
        |> applyTransformation node.transformation
        |> applyFontSize node.fontSize
        |> applyFontFamily node.fontFamily
        |> applyFontColor node.fontColor
        |> applyFontWeight node.fontWeight
        |> applyLetterSpacing node.letterSpacing
        |> applyWordSpacing node.wordSpacing
        |> applyTextAlign node.textAlignment
        |> applyBackground node.background
        |> applyBackgroundColor node.backgroundColor



-- applyExplain attrs =
--     E.explain Debug.todo :: attrs


applyTransformation : Transformation -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyTransformation value attrs =
    -- TODO scale
    attrs
        |> applyOffsetX value
        |> applyOffsetY value
        |> applyRotation value


applyOffsetX { offsetX } attrs =
    if offsetX > 0 then
        E.moveRight offsetX :: attrs

    else if offsetX < 0 then
        E.moveLeft (abs offsetX) :: attrs

    else
        attrs


applyOffsetY { offsetY } attrs =
    if offsetY > 0 then
        E.moveDown offsetY :: attrs

    else if offsetY < 0 then
        E.moveUp (abs offsetY) :: attrs

    else
        attrs


applyRotation { rotation } attrs =
    if rotation > 0 then
        E.rotate (degrees rotation) :: attrs

    else
        attrs


applySpacing : Spacing -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applySpacing value attrs =
    case value of
        SpaceEvenly ->
            E.spaceEvenly :: attrs

        Spacing ( x, y ) ->
            E.spacingXY x y :: attrs


applyWidth : Length -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyWidth value attrs =
    -- TODO merge with applyHeight
    case value.strategy of
        Px value_ ->
            (E.px value_
                |> applyMinLength value.min
                |> applyMaxLength value.max
                |> E.width
            )
                :: attrs

        Content ->
            (E.shrink
                |> applyMinLength value.min
                |> applyMaxLength value.max
                |> E.width
            )
                :: attrs

        Fill portion ->
            (E.fillPortion portion
                |> applyMinLength value.min
                |> applyMaxLength value.max
                |> E.width
            )
                :: attrs

        Unspecified ->
            attrs


applyHeight : Length -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyHeight value attrs =
    case value.strategy of
        Px value_ ->
            (E.px value_
                |> applyMinLength value.min
                |> applyMaxLength value.max
                |> E.height
            )
                :: attrs

        Content ->
            (E.shrink
                |> applyMinLength value.min
                |> applyMaxLength value.max
                |> E.height
            )
                :: attrs

        Fill portion ->
            (E.fillPortion portion
                |> applyMinLength value.min
                |> applyMaxLength value.max
                |> E.height
            )
                :: attrs

        Unspecified ->
            attrs


applyMinLength : Maybe Int -> E.Length -> E.Length
applyMinLength value length =
    case value of
        Just value_ ->
            E.minimum value_ length

        Nothing ->
            length


applyMaxLength : Maybe Int -> E.Length -> E.Length
applyMaxLength value length =
    case value of
        Just value_ ->
            E.maximum value_ length

        Nothing ->
            length


applyAlignX : Alignment -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyAlignX value attrs =
    case value of
        Start ->
            E.alignLeft :: attrs

        Center ->
            E.centerX :: attrs

        End ->
            E.alignRight :: attrs

        None ->
            attrs


applyAlignY : Alignment -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyAlignY value attrs =
    case value of
        Start ->
            E.alignTop :: attrs

        Center ->
            E.centerY :: attrs

        End ->
            E.alignBottom :: attrs

        None ->
            attrs


applyBorderWidth : BorderWidth -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyBorderWidth value attrs =
    Border.widthEach
        { top = value.top
        , right = value.right
        , bottom = value.bottom
        , left = value.left
        }
        :: attrs


applyBorderCorner : BorderCorner -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyBorderCorner value attrs =
    Border.roundEach
        { topLeft = value.topLeft
        , topRight = value.topRight
        , bottomRight = value.bottomRight
        , bottomLeft = value.bottomLeft
        }
        :: attrs


applyBorderColor : Color -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyBorderColor value attrs =
    Border.color value :: attrs


applyPadding : Padding -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyPadding value attrs =
    E.paddingEach
        { top = value.top
        , right = value.right
        , bottom = value.bottom
        , left = value.left
        }
        :: attrs


applyLetterSpacing : Float -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyLetterSpacing value attrs =
    Font.letterSpacing value :: attrs


applyWordSpacing : Float -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyWordSpacing value attrs =
    Font.wordSpacing value :: attrs


applyTextAlign : TextAlignment -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyTextAlign value attrs =
    case value of
        TextLeft ->
            -- Default
            --Font.alignLeft :: attrs
            attrs

        TextCenter ->
            Font.center :: attrs

        TextRight ->
            Font.alignRight :: attrs

        TextJustify ->
            Font.justify :: attrs


applyFontFamily : Local FontFamily -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyFontFamily value attrs =
    case value of
        Local family ->
            let
                family_ =
                    case family.type_ of
                        Native fontStack ->
                            List.map Font.typeface fontStack

                        External _ ->
                            [ Font.typeface family.name ]
            in
            Font.family family_ :: attrs

        Inherit ->
            attrs


applyFontColor : Local Color -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyFontColor value attrs =
    case value of
        Local color ->
            Font.color color :: attrs

        Inherit ->
            attrs


applyFontSize : Local Int -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyFontSize value attrs =
    case value of
        Local size ->
            Font.size size :: attrs

        Inherit ->
            attrs


applyFontWeight : FontWeight -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyFontWeight value attrs =
    case value of
        Heavy ->
            Font.heavy :: attrs

        HeavyItalic ->
            Font.heavy :: Font.italic :: attrs

        ExtraBold ->
            Font.extraBold :: attrs

        ExtraBoldItalic ->
            Font.extraBold :: Font.italic :: attrs

        Bold ->
            Font.bold :: attrs

        BoldItalic ->
            Font.bold :: Font.italic :: attrs

        SemiBold ->
            Font.semiBold :: attrs

        SemiBoldItalic ->
            Font.semiBold :: Font.italic :: attrs

        Medium ->
            Font.medium :: attrs

        MediumItalic ->
            Font.medium :: Font.italic :: attrs

        Regular ->
            Font.regular :: attrs

        Italic ->
            Font.italic :: attrs

        Light ->
            Font.light :: attrs

        LightItalic ->
            Font.light :: Font.italic :: attrs

        ExtraLight ->
            Font.extraLight :: attrs

        ExtraLightItalic ->
            Font.extraLight :: Font.italic :: attrs

        Hairline ->
            Font.hairline :: attrs

        HairlineItalic ->
            Font.hairline :: Font.italic :: attrs


applyBackground : Background -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyBackground value attrs =
    case value of
        Background.Cropped url ->
            Background.image url :: attrs

        Background.Uncropped url ->
            Background.uncropped url :: attrs

        Background.Tiled url ->
            Background.tiled url :: attrs

        Background.None ->
            attrs


applyBackgroundColor : Maybe Color -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyBackgroundColor value attrs =
    case value of
        Just value_ ->
            Background.color value_ :: attrs

        Nothing ->
            attrs


forceBackgroundColor value attrs =
    -- When we use in-place text editor we want to override Elm UI white background
    --   color for multiline input if and only if the element being edited
    --   doesn't have a specified background color
    if value == Nothing then
        applyBackgroundColor (Just Palette.transparent) attrs

    else
        attrs



-- HELPERS


options : List RenderedNode -> List (Option NodeId Msg)
options children =
    List.foldr
        (\node accum ->
            case node of
                RenderedOption el ->
                    el :: accum

                _ ->
                    -- Ignore everything else
                    accum
        )
        []
        children


elements : List RenderedNode -> List (Element Msg)
elements children =
    List.foldr
        (\node accum ->
            case node of
                RenderedElement el ->
                    el :: accum

                _ ->
                    -- Ignore everything else
                    accum
        )
        []
        children


elementId node =
    E.htmlAttribute (A.id (Document.nodeId node.id))


elementClasses ctx node selected =
    let
        dropId =
            AppendTo node.id
    in
    E.htmlAttribute
        (A.classList
            [ ( "element", True )
            , ( "element--dropped", isDroppingInto dropId ctx.dragDrop )
            , ( "element--selected", selected )
            , ( "dragging--file", isDroppingFileInto node.id ctx.fileDrop )
            ]
        )


textEditor attrs text =
    Input.multiline attrs
        { onChange = TextChanged
        , text = text
        , placeholder = Nothing
        , label = Input.labelHidden "Text"
        , spellcheck = False
        }


isDroppingInto dropId dragDrop =
    case DragDrop.getDropId dragDrop of
        Just dropId_ ->
            dropId_ == dropId

        Nothing ->
            False


isDroppingFileInto nodeId fileDrop =
    case fileDrop of
        DraggingOn nodeId_ ->
            nodeId_ == nodeId

        _ ->
            False


placeholderText label =
    -- Center to make it work for rows, columns and text columns
    E.el
        [ Font.color Palette.lightCharcoal
        , Font.center
        , E.centerX
        , E.padding 8
        , E.width E.fill
        , Border.width 1
        , Border.dotted
        , Border.color Palette.lightCharcoal
        ]
        (E.text label)


onClick msg =
    E.htmlAttribute (Html.Events.stopPropagationOn "click" (Decode.succeed ( msg, True )))


onDoubleClick msg =
    E.htmlAttribute (Html.Events.stopPropagationOn "dblclick" (Decode.succeed ( msg, True )))


makeDroppableIf pred dropId attrs =
    if pred then
        attrs
            ++ (DragDrop.droppable DragDropMsg dropId
                    |> List.map E.htmlAttribute
               )

    else
        attrs


makeFileDroppableIf pred nodeId attrs =
    if pred then
        attrs
            ++ ([ on "dragenter" (Decode.succeed (FileDragging nodeId))
                , on "dragover" (Decode.succeed (FileDragging nodeId))
                , on "dragleave" (Decode.succeed FileDragCanceled)
                , on "drop" (fileDropDecoder nodeId)
                ]
                    |> List.map E.htmlAttribute
               )

    else
        attrs


{-| Stop given event and prevent default behavior.

    1. preventDefault does not allow browser to display dropped 
        image on viewport, replacing app UI altogether
    2. stopPropagation allows to have any nested container to 
        handle its own drag events
-}
on : String -> Decoder msg -> H.Attribute msg
on name decoder =
    decoder
        |> Decode.map
            (\msg ->
                { message = msg
                , stopPropagation = True
                , preventDefault = True
                }
            )
        |> Html.Events.custom name


fileDropDecoder : NodeId -> Decoder Msg
fileDropDecoder nodeId =
    Decode.at [ "dataTransfer", "files" ] (Decode.oneOrMore (FileDropped nodeId) File.decoder)
