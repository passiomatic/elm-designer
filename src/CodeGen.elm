module CodeGen exposing (backgroundModule, emit)

{-| Generate Elm code for a given tree node.
-}

import Document exposing (..)
import Element exposing (Color)
import Elm.CodeGen as G exposing (Expression)
import Elm.Pretty
import Palette
import Pretty
import Set exposing (Set)
import String exposing (trim)
import Style.Background as Background exposing (Background)
import Style.Border as Border exposing (BorderCorner, BorderStyle(..), BorderWidth)
import Style.Font as Font exposing (..)
import Style.Input as Input exposing (LabelPosition(..))
import Style.Layout as Layout exposing (Alignment(..), Length(..), Padding, Position(..), Spacing(..))
import Style.Shadow as Shadow exposing (Shadow, ShadowType(..))
import Style.Theme as Theme exposing (Theme)
import Tree as T exposing (Tree)


type EmittedNode
    = EmittedNode Position Expression


debugModule =
    [ "Debug" ]


htmlModule =
    [ "Html" ]


browserModule =
    [ "Browser" ]


elementModule =
    [ "Element" ]


fontModule =
    [ "Font" ]


backgroundModule =
    [ "Background" ]


inputModule =
    [ "Input" ]


borderModule =
    [ "Border" ]


regionModule =
    [ "Region" ]


emit : Theme -> Viewport -> Tree Node -> String
emit theme viewport tree =
    let
        module_ =
            G.normalModule [ "Main" ] [ G.funExpose "main" ]

        imports =
            [ G.importStmt browserModule Nothing Nothing
            , G.importStmt htmlModule Nothing Nothing
            , G.importStmt elementModule Nothing Nothing
            , G.importStmt (elementModule ++ fontModule) (Just fontModule) Nothing
            , G.importStmt (elementModule ++ inputModule) (Just inputModule) Nothing
            , G.importStmt (elementModule ++ backgroundModule) (Just backgroundModule) Nothing
            , G.importStmt (elementModule ++ borderModule) (Just borderModule) Nothing
            , G.importStmt (elementModule ++ regionModule) (Just regionModule) Nothing
            ]

        msgs =
            G.customTypeDecl
                Nothing
                "Msg"
                []
                [ ( "CheckboxClicked", [ G.boolAnn ] )
                , ( "RadioClicked", [ G.intAnn ] )
                , ( "TextChanged", [ G.stringAnn ] )
                ]

        decls =
            [ emitView theme viewport tree
            , msgs
            , emitUpdate
            , G.valDecl
                Nothing
                Nothing
                "init"
                G.unit
            , G.valDecl
                Nothing
                Nothing
                "main"
                (G.apply
                    [ G.fqFun browserModule "sandbox"
                    , G.record
                        [ ( "init", G.val "init" )
                        , ( "view", G.val "view" )
                        , ( "update", G.val "update" )
                        ]
                    ]
                )
            ]

        comments =
            [ emitFontComment tree
            ]

        file =
            G.file module_ imports decls comments
    in
    Elm.Pretty.pretty file
        |> Pretty.pretty 80


emitView theme viewport tree =
    let
        emitMaxWidth =
            case viewport of
                DeviceModel name ->
                    let
                        ( w, _, _ ) =
                            Document.findDeviceInfo name
                    in
                    [ G.apply
                        [ G.fqFun elementModule "width"
                        , G.parens
                            (G.pipe (G.fqFun elementModule "fill")
                                [ G.apply [ G.fqFun elementModule "maximum", G.int w ]
                                ]
                            )
                        ]
                    ]

                _ ->
                    []
    in
    G.funDecl
        Nothing
        Nothing
        "view"
        [ G.varPattern "model"
        ]
        (G.apply
            [ G.fqFun elementModule "layout"
            , G.list
                emitMaxWidth
            , G.parens (root (T.restructure identity (emitNode theme) tree))
            ]
        )


root (EmittedNode _ node) =
    node


emitUpdate =
    G.funDecl
        Nothing
        Nothing
        "update"
        [ G.varPattern "msg"
        , G.varPattern "model"
        ]
        (G.caseExpr (G.val "msg")
            [ ( G.namedPattern "CheckboxClicked"
                    [ G.varPattern "value"
                    ]
              , G.apply
                    [ G.fqFun debugModule "log"
                    , G.string "Checkbox clicked"
                    , G.val "model"
                    ]
              )
            , ( G.namedPattern "RadioClicked"
                    [ G.varPattern "value"
                    ]
              , G.apply
                    [ G.fqFun debugModule "log"
                    , G.string "Radio clicked"
                    , G.val "model"
                    ]
              )
            , ( G.namedPattern "TextChanged"
                    [ G.varPattern "value"
                    ]
              , G.apply
                    [ G.fqFun debugModule "log"
                    , G.string "Text changed"
                    , G.val "model"
                    ]
              )
            ]
        )


emitFontComment tree =
    let
        urls =
            emitFontLinks tree
    in
    if Set.isEmpty urls then
        ""

    else
        "{- The page is using Google Fonts.\n\nTo view fonts correctly don't forget to add this HTML code to the page <head>:\n\n"
            ++ String.join "\n" (Set.toList urls)
            ++ "\n\n-}"


emitFontLinks : Tree Node -> Set String
emitFontLinks tree =
    T.foldl
        (\node accum ->
            case node.fontFamily of
                Local family ->
                    case family.type_ of
                        External url ->
                            ("<link rel='stylesheet' href='" ++ url ++ "'>") :: accum

                        Native _ ->
                            -- Ignore native fonts
                            accum

                Inherit ->
                    -- Ignore inherited fonts
                    accum
        )
        []
        tree
        |> Set.fromList


emitNode : Theme -> Node -> List EmittedNode -> EmittedNode
emitNode theme node children =
    (case node.type_ of
        DocumentNode ->
            emitPage node children

        -- TODO
        PageNode ->
            emitPage node children

        ParagraphNode data ->
            emitParagraph node data

        TextNode data ->
            emitText node data

        ImageNode image ->
            emitImage node image

        HeadingNode data ->
            emitHeading node data

        ColumnNode ->
            emitColumn node children

        TextColumnNode ->
            emitTextColumn node children

        RowNode data ->
            emitRow node data children

        ButtonNode data ->
            emitButton node data

        CheckboxNode data ->
            emitCheckbox theme node data

        TextFieldNode data ->
            emitTextField theme node data

        TextFieldMultilineNode data ->
            emitTextFieldMultiline theme node data

        RadioNode data ->
            emitRadio theme node data children

        OptionNode data ->
            emitOption node data
    )
        |> EmittedNode node.position


emitPage : Node -> List EmittedNode -> Expression
emitPage node children =
    let
        emitter attrs children_ =
            G.apply
                [ G.fqFun elementModule "column"
                , G.list
                    (attrs
                        |> emitStyles node
                    )
                , G.list children_
                ]
    in
    addChildrenFor emitter children


emitColumn : Node -> List EmittedNode -> Expression
emitColumn node children =
    let
        emitter attrs children_ =
            G.apply
                [ G.fqFun elementModule "column"
                , G.list
                    (attrs
                        |> emitStyles node
                    )
                , G.list children_
                ]
    in
    addChildrenFor emitter children


emitTextColumn : Node -> List EmittedNode -> Expression
emitTextColumn node children =
    let
        emitter attrs children_ =
            G.apply
                [ G.fqFun elementModule "textColumn"
                , G.list
                    (attrs
                        |> emitStyles node
                    )
                , G.list children_
                ]
    in
    addChildrenFor emitter children


emitRow : Node -> RowData -> List EmittedNode -> Expression
emitRow node { wrapped } children =
    let
        emitter attrs children_ =
            G.apply
                [ G.fqFun elementModule
                    (if wrapped then
                        "wrappedRow"

                     else
                        "row"
                    )
                , G.list
                    (attrs
                        |> emitStyles node
                    )
                , G.list children_
                ]
    in
    addChildrenFor emitter children


emitParagraph : Node -> TextData -> Expression
emitParagraph node { text } =
    G.apply
        [ G.fqFun elementModule "paragraph"
        , G.list
            ([]
                |> emitStyles node
            )
        , emitLines text
        ]


emitLines : String -> Expression
emitLines text =
    String.lines text
        |> List.map (\line -> G.apply [ G.fqFun elementModule "text", G.string line ])
        |> List.intersperse break
        |> G.list


break =
    G.apply
        [ G.fqFun elementModule "html"
        , G.parens
            (G.apply
                [ G.fqFun htmlModule "br"
                , G.list []
                , G.list []
                ]
            )
        ]


emitText : Node -> TextData -> Expression
emitText node { text } =
    G.apply
        [ G.fqFun elementModule "el"
        , G.list
            ([]
                |> emitStyles node
            )
        , G.parens (G.apply [ G.fqFun elementModule "text", G.string text ])
        ]


emitButton : Node -> TextData -> Expression
emitButton node { text } =
    G.apply
        [ G.fqFun inputModule "button"
        , G.list
            ([]
                |> emitStyles node
            )
        , G.record
            [ ( "onPress", G.val "Nothing" )
            , ( "label", G.apply [ G.fqFun elementModule "text", G.string text ] )
            ]
        ]


emitCheckbox : Theme -> Node -> LabelData -> Expression
emitCheckbox theme node label =
    let
        labelPadding =
            { zero | left = Theme.xsmall theme }
    in
    G.apply
        [ G.fqFun inputModule "checkbox"
        , G.list
            ([]
                |> emitStyles node
            )
        , G.record
            [ ( "onChange", G.val "CheckboxClicked" )
            , ( "icon", G.fqFun inputModule "defaultCheckbox" )
            , ( "checked", G.val "False" )
            , ( "label"
              , G.apply
                    [ emitLabelPosition label.position
                    , G.list
                        []
                    , G.parens (G.apply [ G.fqFun elementModule "text", G.string label.text ])
                    ]
              )
            ]
        ]


emitTextField : Theme -> Node -> LabelData -> Expression
emitTextField theme node label =
    G.apply
        [ G.fqFun inputModule "text"
        , G.list
            ([]
                |> emitStyles node
            )
        , G.record
            [ ( "onChange", G.val "TextChanged" )
            , ( "text", G.string "" )
            , ( "placeholder", G.val "Nothing" )
            , ( "label"
              , G.apply
                    [ emitLabelPosition label.position
                    , G.list
                        ([]
                            |> emitFontColor label.color
                        )
                    , G.parens (G.apply [ G.fqFun elementModule "text", G.string label.text ])
                    ]
              )
            ]
        ]


emitTextFieldMultiline : Theme -> Node -> LabelData -> Expression
emitTextFieldMultiline theme node label =
    G.apply
        [ G.fqFun inputModule "multiline"
        , G.list
            ([]
                |> emitStyles node
            )
        , G.record
            [ ( "onChange", G.val "TextChanged" )
            , ( "text", G.string "" )
            , ( "placeholder", G.val "Nothing" )
            , ( "spellcheck", G.val "False" )
            , ( "label"
              , G.apply
                    [ emitLabelPosition label.position
                    , G.list
                        ([]
                            |> emitFontColor label.color
                        )
                    , G.parens (G.apply [ G.fqFun elementModule "text", G.string label.text ])
                    ]
              )
            ]
        ]


emitRadio : Theme -> Node -> LabelData -> List EmittedNode -> Expression
emitRadio theme node label children =
    let
        emitter attrs children_ =
            G.apply
                [ G.fqFun inputModule "radio"
                , G.list
                    (attrs
                        |> emitStyles node
                    )
                , G.record
                    [ ( "onChange", G.val "RadioClicked" )
                    , ( "selected", G.val "Nothing" )
                    , ( "label"
                      , G.apply
                            [ emitLabelPosition label.position
                            , G.list
                                [ G.apply [ G.fqFun fontModule "color", G.parens (emitColor theme.labelColor) ]
                                ]
                            , G.parens (G.apply [ G.fqFun elementModule "text", G.string label.text ])
                            ]
                      )
                    , ( "options", G.list children_ )
                    ]
                ]
    in
    addChildrenFor emitter children


emitOption : Node -> TextData -> Expression
emitOption node { text } =
    G.apply
        [ G.fqFun inputModule "option"
        , G.int 1
        , G.parens
            (G.apply
                [ G.fqFun elementModule "el"
                , G.list
                    ([]
                        |> emitStyles node
                    )
                , G.parens (G.apply [ G.fqFun elementModule "text", G.string text ])
                ]
            )
        ]


emitImage : Node -> ImageData -> Expression
emitImage node image =
    G.apply
        [ G.fqFun elementModule "image"
        , G.list
            ([]
                |> clipIf (Border.isRounded node.borderCorner)
                |> emitStyles node
            )
        , G.record
            [ ( "src", G.string image.src )
            , ( "description", G.string image.description )
            ]
        ]


emitHeading : Node -> HeadingData -> Expression
emitHeading node { text, level } =
    -- Paragraph allows to wrap text and to set a line height
    G.apply
        [ G.fqFun elementModule "paragraph"
        , G.list
            ([ G.apply [ G.fqFun regionModule "heading", G.int level ]
             ]
                |> emitStyles node
            )
        , emitLines text
        ]



-- ATTRIBUTES


emitStyles : Node -> List Expression -> List Expression
emitStyles node attrs =
    attrs
        |> emitBorder node.borderColor node.borderStyle node.borderWidth
        |> emitCorner node.borderCorner
        |> emitPadding node.padding
        |> emitWidth node.width node.widthMin node.widthMax
        |> emitHeight node.height node.heightMin node.heightMax
        |> emitSpacing node.spacing
        |> emitFontSize node.fontSize
        |> emitFontFamily node.fontFamily
        |> emitFontColor node.fontColor
        |> emitFontWeight node.fontWeight
        |> emitLetterSpacing node.letterSpacing
        |> emitWordSpacing node.wordSpacing
        |> emitTextAlign node.textAlignment
        |> emitAlignX node.alignmentX
        |> emitAlignY node.alignmentY
        |> emitOffsetX node
        |> emitOffsetY node
        |> emitRotation node.rotation
        |> emitScale node.scale
        |> emitBackground node
        |> emitShadow node.shadow


emitShadow : Shadow -> List Expression -> List Expression
emitShadow value attrs =
    if value.offsetX == 0 && value.offsetY == 0 && value.size == 0 && value.blur == 0 then
        attrs

    else
        G.apply
            [ G.fqFun borderModule "shadow"
            , G.record
                [ ( "offset", G.tuple [ G.float value.offsetX, G.float value.offsetY ] )
                , ( "size", G.float value.size )
                , ( "blur", G.float value.blur )
                , ( "color", emitColor value.color )
                ]
            ]
            :: attrs


emitOffsetX : Node -> List Expression -> List Expression
emitOffsetX node attrs =
    case node.type_ of
        PageNode ->
            -- Offsets are used to render page on workspace, do no emit
            attrs

        _ ->
            if node.offsetX < 0 then
                G.apply [ G.fqFun elementModule "moveLeft", G.float (abs node.offsetX) ] :: attrs

            else if node.offsetX > 0 then
                G.apply [ G.fqFun elementModule "moveRight", G.float node.offsetX ] :: attrs

            else
                attrs


emitOffsetY : Node -> List Expression -> List Expression
emitOffsetY node attrs =
    case node.type_ of
        PageNode ->
            -- Offsets are used to render page on workspace, do no emit
            attrs

        _ ->
            if node.offsetY < 0 then
                G.apply [ G.fqFun elementModule "moveUp", G.float (abs node.offsetY) ] :: attrs

            else if node.offsetY > 0 then
                G.apply [ G.fqFun elementModule "moveDown", G.float node.offsetY ] :: attrs

            else
                attrs


emitRotation : Float -> List Expression -> List Expression
emitRotation value attrs =
    if value /= 0 then
        G.apply [ G.fqFun elementModule "rotate", G.float value ] :: attrs

    else
        attrs


emitScale : Float -> List Expression -> List Expression
emitScale value attrs =
    if value /= 1.0 then
        G.apply [ G.fqFun elementModule "scale", G.float value ] :: attrs

    else
        attrs


emitPadding : Padding -> List Expression -> List Expression
emitPadding value attrs =
    if value.top == 0 && value.right == 0 && value.bottom == 0 && value.left == 0 then
        attrs

    else if value.top == value.bottom && value.right == value.left then
        G.apply
            [ G.fqFun elementModule "paddingXY"
            , G.int value.right
            , G.int value.top
            ]
            :: attrs

    else
        G.apply
            [ G.fqFun elementModule "paddingEach"
            , G.record
                [ ( "top", G.int value.top )
                , ( "right", G.int value.right )
                , ( "bottom", G.int value.bottom )
                , ( "left", G.int value.left )
                ]
            ]
            :: attrs


emitBorder : Color -> BorderStyle -> BorderWidth -> List Expression -> List Expression
emitBorder borderColor borderStyle borderWidth attrs =
    if borderWidth.top == 0 && borderWidth.right == 0 && borderWidth.bottom == 0 && borderWidth.left == 0 then
        attrs

    else
        G.apply
            [ G.fqFun borderModule "color"
            , G.parens
                (emitColor borderColor)
            ]
            :: (case borderStyle of
                    Solid ->
                        G.fqFun borderModule "solid"

                    Dashed ->
                        G.fqFun borderModule "dashed"

                    Dotted ->
                        G.fqFun borderModule "dotted"
               )
            :: (if borderWidth.top == borderWidth.bottom && borderWidth.right == borderWidth.left then
                    G.apply
                        [ G.fqFun borderModule "widthXY"
                        , G.int borderWidth.right
                        , G.int borderWidth.top
                        ]

                else
                    G.apply
                        [ G.fqFun borderModule "widthEach"
                        , G.record
                            [ ( "top", G.int borderWidth.top )
                            , ( "right", G.int borderWidth.right )
                            , ( "bottom", G.int borderWidth.bottom )
                            , ( "left", G.int borderWidth.left )
                            ]
                        ]
               )
            :: attrs


emitCorner : BorderCorner -> List Expression -> List Expression
emitCorner borderCorner attrs =
    if borderCorner.topLeft == 0 && borderCorner.topRight == 0 && borderCorner.bottomLeft == 0 && borderCorner.bottomRight == 0 then
        attrs

    else if borderCorner.locked then
        G.apply
            [ G.fqFun borderModule "rounded"
            , G.int borderCorner.topLeft
            ]
            :: attrs

    else
        G.apply
            [ G.fqFun borderModule "roundEach"
            , G.record
                [ ( "topLeft", G.int borderCorner.topLeft )
                , ( "topRight", G.int borderCorner.topRight )
                , ( "bottomLeft", G.int borderCorner.bottomLeft )
                , ( "bottomRight", G.int borderCorner.bottomRight )
                ]
            ]
            :: attrs


emitColor value =
    let
        rgba =
            Element.toRgb value
    in
    G.apply
        [ G.fqFun elementModule "rgba255"
        , G.int <| round (rgba.red * 255)
        , G.int <| round (rgba.green * 255)
        , G.int <| round (rgba.blue * 255)
        , G.float rgba.alpha
        ]


emitFontFamily : Local FontFamily -> List Expression -> List Expression
emitFontFamily value attrs =
    case value of
        Local value_ ->
            case value_.type_ of
                Native fontStack ->
                    G.apply
                        [ G.fqFun fontModule "family"
                        , G.list
                            (List.map
                                (\name ->
                                    G.apply
                                        [ G.fqFun fontModule "typeface"
                                        , G.string name
                                        ]
                                )
                                fontStack
                            )
                        ]
                        :: attrs

                External url ->
                    G.apply
                        [ G.fqFun fontModule "family"
                        , G.list
                            [ G.apply
                                [ G.fqFun fontModule "typeface"
                                , G.string value_.name
                                ]
                            ]
                        ]
                        :: attrs

        Inherit ->
            attrs


emitFontColor : Local Color -> List Expression -> List Expression
emitFontColor value attrs =
    case value of
        Local value_ ->
            G.apply
                [ G.fqFun fontModule "color"
                , G.parens
                    (emitColor value_)
                ]
                :: attrs

        Inherit ->
            attrs


emitFontWeight : FontWeight -> List Expression -> List Expression
emitFontWeight value attrs =
    let
        emit_ w =
            G.apply
                [ G.fqFun fontModule w
                ]
    in
    case value of
        Heavy ->
            emit_ "heavy" :: attrs

        HeavyItalic ->
            emit_ "heavy" :: emit_ "italic" :: attrs

        ExtraBold ->
            emit_ "extraBold" :: attrs

        ExtraBoldItalic ->
            emit_ "extraBold" :: emit_ "italic" :: attrs

        Bold ->
            emit_ "bold" :: attrs

        BoldItalic ->
            emit_ "bold" :: emit_ "italic" :: attrs

        SemiBold ->
            emit_ "semiBold" :: attrs

        SemiBoldItalic ->
            emit_ "semiBold" :: emit_ "italic" :: attrs

        Medium ->
            emit_ "medium" :: attrs

        MediumItalic ->
            emit_ "medium" :: emit_ "italic" :: attrs

        Regular ->
            --emit_ "regular" :: attrs
            -- This is the defauilt for all fonts
            attrs

        Italic ->
            emit_ "italic" :: attrs

        Light ->
            emit_ "light" :: attrs

        LightItalic ->
            emit_ "light" :: emit_ "italic" :: attrs

        ExtraLight ->
            emit_ "extraLight" :: attrs

        ExtraLightItalic ->
            emit_ "extraLight" :: emit_ "italic" :: attrs

        Hairline ->
            emit_ "hairline" :: attrs

        HairlineItalic ->
            emit_ "extraLight" :: emit_ "italic" :: attrs


emitSpacing : Spacing -> List Expression -> List Expression
emitSpacing value attrs =
    case value of
        SpaceEvenly ->
            G.fqFun elementModule "spaceEvenly" :: attrs

        Spacing ( x, y ) ->
            if x == 0 && y == 0 then
                attrs

            else
                G.apply [ G.fqFun elementModule "spacingXY", G.int x, G.int y ] :: attrs


emitWidth : Length -> Maybe Int -> Maybe Int -> List Expression -> List Expression
emitWidth value min max attrs =
    emitLength value min max (G.fqFun elementModule "width") attrs


emitHeight : Length -> Maybe Int -> Maybe Int -> List Expression -> List Expression
emitHeight value min max attrs =
    emitLength value min max (G.fqFun elementModule "height") attrs


emitLength : Length -> Maybe Int -> Maybe Int -> Expression -> List Expression -> List Expression
emitLength value min max fun attrs =
    case value of
        Px px ->
            G.apply
                [ fun
                , G.parens
                    (G.pipe (G.apply [ G.fqFun elementModule "px", G.int px ])
                        ([]
                            |> emitMinLength min
                            |> emitMaxLength max
                        )
                    )
                ]
                :: attrs

        Content ->
            G.apply
                [ fun
                , G.parens
                    (G.pipe (G.fqFun elementModule "shrink")
                        ([]
                            |> emitMinLength min
                            |> emitMaxLength max
                        )
                    )
                ]
                :: attrs

        Fill portion ->
            (if portion == 1 then
                G.apply
                    [ fun
                    , G.parens
                        (G.pipe (G.fqFun elementModule "fill")
                            ([]
                                |> emitMinLength min
                                |> emitMaxLength max
                            )
                        )
                    ]

             else
                G.apply
                    [ fun
                    , G.parens
                        (G.pipe (G.apply [ G.fqFun elementModule "fillPortion", G.int portion ])
                            ([]
                                |> emitMinLength min
                                |> emitMaxLength max
                            )
                        )
                    ]
            )
                :: attrs

        Unspecified ->
            -- Emit "shrink" as default to specify min/max values.
            --   This is probably fixed in Elm UI 2 since we can emit min/max
            --   values indipendently of element width/height values
            G.apply
                [ fun
                , G.parens
                    (G.pipe (G.fqFun elementModule "shrink")
                        ([]
                            |> emitMinLength min
                            |> emitMaxLength max
                        )
                    )
                ]
                :: attrs


emitMinLength : Maybe Int -> List Expression -> List Expression
emitMinLength value attrs =
    case value of
        Just value_ ->
            G.apply [ G.fqFun elementModule "minimum", G.int value_ ] :: attrs

        Nothing ->
            attrs


emitMaxLength : Maybe Int -> List Expression -> List Expression
emitMaxLength value attrs =
    case value of
        Just value_ ->
            G.apply [ G.fqFun elementModule "maximum", G.int value_ ] :: attrs

        Nothing ->
            attrs


emitAlignX : Alignment -> List Expression -> List Expression
emitAlignX value attrs =
    case value of
        Start ->
            G.fqFun elementModule "alignLeft" :: attrs

        Center ->
            G.fqFun elementModule "centerX" :: attrs

        End ->
            G.fqFun elementModule "alignRight" :: attrs

        None ->
            attrs


emitAlignY : Alignment -> List Expression -> List Expression
emitAlignY value attrs =
    case value of
        Start ->
            G.fqFun elementModule "alignTop" :: attrs

        Center ->
            G.fqFun elementModule "centerY" :: attrs

        End ->
            G.fqFun elementModule "alignBottom" :: attrs

        None ->
            attrs


emitFontSize : Local Int -> List Expression -> List Expression
emitFontSize value attrs =
    case value of
        Local size ->
            G.apply [ G.fqFun fontModule "size", G.int size ] :: attrs

        Inherit ->
            attrs


emitLetterSpacing : Float -> List Expression -> List Expression
emitLetterSpacing value attrs =
    if value /= 0 then
        G.apply [ G.fqFun fontModule "letterSpacing", G.float value ] :: attrs

    else
        attrs


emitWordSpacing : Float -> List Expression -> List Expression
emitWordSpacing value attrs =
    if value /= 0 then
        G.apply [ G.fqFun fontModule "wordSpacing", G.float value ] :: attrs

    else
        attrs


emitTextAlign : TextAlignment -> List Expression -> List Expression
emitTextAlign value attrs =
    case value of
        TextStart ->
            --G.apply [ G.fqFun fontModule "alignLeft" ] :: attrs
            attrs

        TextCenter ->
            G.apply [ G.fqFun fontModule "center" ] :: attrs

        TextEnd ->
            G.apply [ G.fqFun fontModule "alignRight" ] :: attrs

        TextJustify ->
            G.apply [ G.fqFun fontModule "justify" ] :: attrs


emitBackground : Node -> List Expression -> List Expression
emitBackground node attrs =
    case node.background of
        Background.Image value_ ->
            G.apply [ G.fqFun backgroundModule "image", G.string value_ ] :: attrs

        Background.Solid value_ ->
            emitBackgroundColor value_ :: attrs

        Background.None ->
            case node.type_ of
                -- Override Elm UI defauls for text fields
                TextFieldNode _ ->
                    emitBackgroundColor Palette.transparent :: attrs

                TextFieldMultilineNode _ ->
                    emitBackgroundColor Palette.transparent :: attrs

                _ ->
                    attrs


emitBackgroundColor color =
    G.apply
        [ G.fqFun backgroundModule "color"
        , G.parens
            (emitColor color)
        ]


emitLabelPosition : LabelPosition -> Expression
emitLabelPosition position =
    (case position of
        LabelAbove ->
            "labelAbove"

        LabelBelow ->
            "labelBelow"

        LabelLeft ->
            "labelLeft"

        LabelRight ->
            "labelRight"

        LabelHidden ->
            "labelHidden"
    )
        |> G.fqFun inputModule



-- HELPERS


addChildrenFor emitter children =
    let
        ( newAttrs, newChildren ) =
            List.foldl
                (\child ( accumAttrs, accumChildren ) ->
                    addChild child accumAttrs accumChildren
                )
                ( [], [] )
                children
    in
    emitter newAttrs newChildren


addChild :
    EmittedNode
    -> List Expression
    -> List Expression
    -> ( List Expression, List Expression )
addChild (EmittedNode position child) attrs siblings =
    case position of
        Above ->
            ( G.apply
                [ G.fqFun elementModule "above"
                , G.parens child
                ]
                :: attrs
            , siblings
            )

        Below ->
            ( G.apply
                [ G.fqFun elementModule "below"
                , G.parens child
                ]
                :: attrs
            , siblings
            )

        OnStart ->
            ( G.apply
                [ G.fqFun elementModule "onLeft"
                , G.parens child
                ]
                :: attrs
            , siblings
            )

        OnEnd ->
            ( G.apply
                [ G.fqFun elementModule "onRight"
                , G.parens child
                ]
                :: attrs
            , siblings
            )

        InFront ->
            ( G.apply
                [ G.fqFun elementModule "inFront"
                , G.parens child
                ]
                :: attrs
            , siblings
            )

        BehindContent ->
            ( G.apply
                [ G.fqFun elementModule "behindContent"
                , G.parens child
                ]
                :: attrs
            , siblings
            )

        Normal ->
            -- Do not reverse children list
            ( attrs, siblings ++ [ child ] )


clipIf pred attrs =
    if pred then
        G.fqFun elementModule "clip" :: attrs

    else
        attrs


zero =
    Layout.padding 0
