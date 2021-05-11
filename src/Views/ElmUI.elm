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
      --| RenderedPage (Html Msg)
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
            renderEditableText ctx node selected data.text renderParagraph

        HeadingNode data ->
            -- Render as paragraph so we have text wrapping and line spacing settings
            renderEditableText ctx node selected data.text renderParagraph

        TextNode data ->
            renderEditableText ctx node selected data.text renderText

        ImageNode data ->
            renderImage ctx node selected data

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
        renderer attrs =
            let
                newAttrs =
                    attrs
                        |> makeFileDroppableIf (not <| Common.isDragging ctx.dragDrop) node.id

                newChildren =
                    if List.length children == 0 then
                        [ placeholderText "Empty Text Column" ]

                    else
                        elements children
            in
            E.textColumn newAttrs newChildren
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


renderColumn : Context -> Node -> Bool -> List RenderedNode -> RenderedNode
renderColumn ctx node selected children =
    let
        renderer attrs =
            let
                newAttrs =
                    attrs
                        |> makeFileDroppableIf (not <| Common.isDragging ctx.dragDrop) node.id

                newChildren =
                    if List.length children == 0 then
                        [ placeholderText "Empty Column" ]

                    else
                        elements children
            in
            E.column newAttrs newChildren
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


renderRow : Context -> Node -> Bool -> RowData -> List RenderedNode -> RenderedNode
renderRow ctx node selected { wrapped } children =
    let
        renderer attrs =
            let
                newAttrs =
                    attrs
                        |> makeFileDroppableIf (not <| Common.isDragging ctx.dragDrop) node.id

                newChildren =
                    if List.length children == 0 then
                        [ placeholderText "Empty Row" ]

                    else
                        elements children
            in
            if wrapped then
                E.wrappedRow newAttrs newChildren

            else
                E.row newAttrs newChildren
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


{-| Render page as Elm UI column to layout elements vertically one after another, just like a regular HTML page.
-}
renderPage : Context -> Node -> Bool -> List RenderedNode -> RenderedNode
renderPage ctx node selected children =
    let
        renderer attrs =
            let
                newAttrs =
                    attrs
                        |> makeFileDroppableIf (not <| Common.isDragging ctx.dragDrop) node.id
            in
            if List.isEmpty children then
                renderEmptyPage attrs

            else
                E.column attrs (elements children)
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


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
        renderer attrs =
            let
                newAttrs =
                    attrs
                        |> clipIf (Style.Border.isRounded node.borderCorner)
            in
            E.image newAttrs image
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


renderEditableText :
    Context
    -> Node
    -> Bool
    -> String
    -> (Node -> List (E.Attribute Msg) -> String -> Element Msg)
    -> RenderedNode
renderEditableText ctx node selected text nodeRenderer =
    let
        renderer attrs =
            case ( ctx.inspector, selected ) of
                ( EditingText, True ) ->
                    -- While editing capture the click events but do nothing so the event won't bubble up
                    textEditor
                        (onDoubleClick NoOp
                            :: onClick NoOp
                            :: attrs
                            |> forceBackgroundColor node.background
                        )
                        text

                _ ->
                    nodeRenderer node attrs text
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


renderParagraph node attrs text =
    E.paragraph
        (onDoubleClick (TextEditingStarted textEditorId)
            :: onClick (NodeSelected node.id)
            :: attrs
        )
        (if String.trim text == "" then
            [ E.text "Double click to edit text" ]

         else
            renderLines text
        )


renderText node attrs text =
    E.el
        (onDoubleClick (TextEditingStarted textEditorId)
            :: onClick (NodeSelected node.id)
            :: attrs
        )
        (if String.trim text == "" then
            E.text "Double click to edit text"

         else
            E.text text
        )


renderLines : String -> List (Element msg)
renderLines text =
    String.lines text
        |> List.map E.text
        |> List.intersperse break


break =
    E.html (H.br [] [])


renderTextField : Context -> Node -> Bool -> LabelData -> RenderedNode
renderTextField ctx node selected label =
    let
        renderer attrs =
            let
                newAttrs =
                    -- Deactivate field while in design mode
                    E.htmlAttribute (A.readonly (ctx.mode == DesignMode))
                        :: attrs
            in
            Input.text
                newAttrs
                { onChange = \s -> NoOp
                , text = ""
                , placeholder = Nothing
                , label = labelPosition label.position [ Font.color ctx.theme.labelColor ] label.text
                }
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


renderTextFieldMultiline : Context -> Node -> Bool -> LabelData -> RenderedNode
renderTextFieldMultiline ctx node selected label =
    let
        renderer attrs =
            let
                newAttrs =
                    -- Deactivate field while in design mode
                    E.htmlAttribute (A.readonly (ctx.mode == DesignMode))
                        :: attrs
            in
            Input.multiline
                newAttrs
                { onChange = \s -> NoOp
                , text = ""
                , placeholder = Nothing
                , spellcheck = False
                , label =
                    labelPosition label.position [ Font.color ctx.theme.labelColor ] label.text
                }
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


renderButton : Context -> Node -> Bool -> TextData -> RenderedNode
renderButton ctx node selected { text } =
    let
        renderer attrs =
            Input.button
                attrs
                { onPress = Nothing
                , label = E.text text
                }
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


renderCheckbox : Context -> Node -> Bool -> LabelData -> RenderedNode
renderCheckbox ctx node selected label =
    let
        renderer attrs =
            Input.checkbox
                attrs
                { onChange = \s -> NoOp
                , icon = Input.defaultCheckbox
                , checked = True
                , label = labelPosition label.position [ Font.color ctx.theme.labelColor ] label.text
                }
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


renderRadio : Context -> Node -> Bool -> LabelData -> List RenderedNode -> RenderedNode
renderRadio ctx node selected label children =
    let
        renderer attrs =
            Input.radio
                attrs
                { onChange = \s -> NoOp
                , selected = Nothing
                , label = labelPosition label.position [ Font.color ctx.theme.labelColor ] label.text
                , options =
                    options children
                }
    in
    wrapElement ctx node selected renderer
        |> RenderedElement


renderOption : Context -> Node -> Bool -> TextData -> RenderedNode
renderOption ctx node selected { text } =
    let
        renderer attrs =
            E.el attrs (E.text text)
    in
    wrapElement ctx node selected renderer
        |> Input.option node.id
        |> RenderedOption



-- STYLES


applyStyles : Node -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyStyles node attrs =
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


applyWidth =
    applyLength E.width


applyHeight =
    applyLength E.height


applyLength : (E.Length -> E.Attribute Msg) -> Length -> Maybe Int -> Maybe Int -> List (E.Attribute Msg) -> List (E.Attribute Msg)
applyLength fn value min max attrs =
    case value of
        Px value_ ->
            (E.px value_
                |> applyMinLength min
                |> applyMaxLength max
                |> fn
            )
                :: attrs

        Content ->
            (E.shrink
                |> applyMinLength min
                |> applyMaxLength max
                |> fn
            )
                :: attrs

        Fill portion ->
            (E.fillPortion portion
                |> applyMinLength min
                |> applyMaxLength max
                |> fn
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



-- applyPosition : Position -> List (E.Attribute Msg) -> List (E.Attribute Msg)
-- applyPosition value attrs =
--     case value of
--         Above ->
--             E.above :: attrs
--         Below ->
--             E.below :: attrs
--         OnStart ->
--             E.onLeft :: attrs
--         OnEnd ->
--             E.onRight :: attrs
--         InFront  ->
--             E.inFront :: attrs
--         BehindContent ->
--             E.behindContent :: attrs
--         Normal ->
--             attrs


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
        Background.Image value_ ->
            if value_ /= "" then
                Background.image value_ :: attrs

            else
                attrs

        Background.Solid value_ ->
            Background.color value_ :: attrs

        Background.None ->
            attrs


forceBackgroundColor value attrs =
    -- When we use in-place text editor we want to override Elm UI white background
    --   color for multiline input if and only if the element being edited
    --   doesn't have a background color already
    case value of
        Background.None ->
            applyBackground (Background.Solid Palette.transparent) attrs

        _ ->
            attrs



-- HELPERS


wrapElement : Context -> Node -> Bool -> (List (E.Attribute Msg) -> Element Msg) -> Element Msg
wrapElement ctx node selected renderer =
    let
        attrs =
            [ E.width E.fill
            , E.height E.fill
            ]
                |> applyStyles node
    in
    E.el
        ([ elementClasses ctx node selected
         , elementId node
         , onClick (NodeSelected node.id)

         --, E.onRight (E.el [ E.centerY, E.moveLeft 14 ] (E.html <| H.div [ A.class "element__connect" ] []))
         --  , E.onRight (E.el [E.alignBottom, E.moveLeft 14 ] (E.html <| H.div [ A.class "element__nudge" ] []))
         --  , E.onRight (E.el [E.alignTop, E.moveLeft 14 ] (E.html <| H.div [ A.class "element__nudge" ] []))
         --  , E.onLeft (E.el [E.alignTop, E.moveRight 14 ] (E.html <| H.div [ A.class "element__nudge" ] []))
         ]
            |> makeDroppableIf (Common.canDropInto node ctx.dragDrop) (AppendTo node.id)
            |> applyWidth node.width node.widthMin node.widthMax
            |> applyHeight node.height node.heightMin node.heightMax
            |> applyAlignX node.alignmentX
            |> applyAlignY node.alignmentY
        )
        (renderer attrs)


labelPosition position attrs text =
    case position of
        LabelAbove ->
            Input.labelAbove attrs (E.text text)

        LabelBelow ->
            Input.labelBelow attrs (E.text text)

        LabelLeft ->
            Input.labelLeft attrs (E.text text)

        LabelRight ->
            Input.labelRight attrs (E.text text)

        LabelHidden ->
            Input.labelHidden text


clipIf pred attrs =
    if pred then
        E.clip :: attrs

    else
        attrs


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


textEditorId =
    "text-editor"


textEditor attrs text =
    let
        newAttrs =
            E.htmlAttribute (A.id textEditorId)
                :: attrs
    in
    Input.multiline newAttrs
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
    -- FIXME: textColumn seems to not honor E.height E.fill
    E.el
        [ Font.color Palette.lightCharcoal
        , E.width E.fill
        , E.height E.fill
        , Border.width 1
        , Border.dotted
        , Border.color Palette.lightCharcoal
        ]
        (E.el
            [ E.centerX
            , E.centerY
            , E.padding 8
            ]
            (E.text label)
        )


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
