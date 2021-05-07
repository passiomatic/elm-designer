module CodeGen exposing (backgroundModule, emit)

{-| Generate Elm code for a given tree node.
-}

import Document exposing (..)
import Element exposing (Color)
import Elm.CodeGen as CodeGen exposing (Expression)
import Elm.Pretty
import Pretty
import Set exposing (Set)
import Style.Background as Background exposing (Background)
import Style.Border as Border exposing (BorderCorner, BorderStyle(..), BorderWidth)
import Style.Font as Font exposing (..)
import Style.Layout as Layout exposing (Alignment(..), Length, Padding, Spacing(..), Length(..), Transformation)
import Style.Theme as Theme exposing (Theme)
import Tree as T exposing (Tree)


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
            CodeGen.normalModule [ "Main" ] [ CodeGen.funExpose "main" ]

        imports =
            [ CodeGen.importStmt browserModule Nothing Nothing
            , CodeGen.importStmt htmlModule Nothing Nothing
            , CodeGen.importStmt elementModule Nothing Nothing
            , CodeGen.importStmt (elementModule ++ fontModule) (Just fontModule) Nothing
            , CodeGen.importStmt (elementModule ++ inputModule) (Just inputModule) Nothing
            , CodeGen.importStmt (elementModule ++ backgroundModule) (Just backgroundModule) Nothing
            , CodeGen.importStmt (elementModule ++ borderModule) (Just borderModule) Nothing
            , CodeGen.importStmt (elementModule ++ regionModule) (Just regionModule) Nothing
            ]

        msgs =
            CodeGen.customTypeDecl
                Nothing
                "Msg"
                []
                [ ( "CheckboxClicked", [ CodeGen.boolAnn ] )
                , ( "RadioClicked", [ CodeGen.intAnn ] )
                , ( "TextChanged", [ CodeGen.stringAnn ] )
                ]

        decls =
            [ emitView theme viewport tree
            , msgs
            , emitUpdate
            , CodeGen.valDecl
                Nothing
                Nothing
                "init"
                CodeGen.unit
            , CodeGen.valDecl
                Nothing
                Nothing
                "main"
                (CodeGen.apply
                    [ CodeGen.fqFun browserModule "sandbox"
                    , CodeGen.record
                        [ ( "init", CodeGen.val "init" )
                        , ( "view", CodeGen.val "view" )
                        , ( "update", CodeGen.val "update" )
                        ]
                    ]
                )
            ]

        comments =
            [ emitFontComment tree
            ]

        file =
            CodeGen.file module_ imports decls comments
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
                    [ CodeGen.apply
                        [ CodeGen.fqFun elementModule "width"
                        , CodeGen.parens
                            (CodeGen.pipe (CodeGen.fqFun elementModule "fill")
                                [ CodeGen.apply [ CodeGen.fqFun elementModule "maximum", CodeGen.int w ]
                                ]
                            )
                        ]
                    ]

                _ ->
                    []
    in
    CodeGen.funDecl
        Nothing
        Nothing
        "view"
        [ CodeGen.varPattern "model"
        ]
        (CodeGen.apply
            [ CodeGen.fqFun elementModule "layout"
            , CodeGen.list
                emitMaxWidth
            , CodeGen.parens (T.restructure identity (emitNode theme) tree)
            ]
        )


emitUpdate =
    CodeGen.funDecl
        Nothing
        Nothing
        "update"
        [ CodeGen.varPattern "msg"
        , CodeGen.varPattern "model"
        ]
        (CodeGen.caseExpr (CodeGen.val "msg")
            [ ( CodeGen.namedPattern "CheckboxClicked"
                    [ CodeGen.varPattern "value"
                    ]
              , CodeGen.apply
                    [ CodeGen.fqFun debugModule "log"
                    , CodeGen.string "Checkbox clicked"
                    , CodeGen.val "model"
                    ]
              )
            , ( CodeGen.namedPattern "RadioClicked"
                    [ CodeGen.varPattern "value"
                    ]
              , CodeGen.apply
                    [ CodeGen.fqFun debugModule "log"
                    , CodeGen.string "Radio clicked"
                    , CodeGen.val "model"
                    ]
              )
            , ( CodeGen.namedPattern "TextChanged"
                    [ CodeGen.varPattern "value"
                    ]
              , CodeGen.apply
                    [ CodeGen.fqFun debugModule "log"
                    , CodeGen.string "Text changed"
                    , CodeGen.val "model"
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


emitNode : Theme -> Node -> List Expression -> Expression
emitNode theme node children =
    case node.type_ of
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


emitPage : Node -> List Expression -> Expression
emitPage node children =
    CodeGen.apply
        [ CodeGen.fqFun elementModule "column"
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , CodeGen.list children
        ]


emitColumn : Node -> List Expression -> Expression
emitColumn node children =
    CodeGen.apply
        [ CodeGen.fqFun elementModule "column"
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , CodeGen.list children
        ]


emitTextColumn : Node -> List Expression -> Expression
emitTextColumn node children =
    CodeGen.apply
        [ CodeGen.fqFun elementModule "textColumn"
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , CodeGen.list children
        ]


emitRow : Node -> RowData -> List Expression -> Expression
emitRow node { wrapped } children =
    CodeGen.apply
        [ CodeGen.fqFun elementModule
            (if wrapped then
                "wrappedRow"

             else
                "row"
            )
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , CodeGen.list children
        ]


emitParagraph : Node -> TextData -> Expression
emitParagraph node { text } =
    CodeGen.apply
        [ CodeGen.fqFun elementModule "paragraph"
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , emitLines text
        ]


emitLines : String -> Expression
emitLines text =
    String.lines text
        |> List.map (\line -> CodeGen.apply [ CodeGen.fqFun elementModule "text", CodeGen.string line ])
        |> List.intersperse break
        |> CodeGen.list


break =
    CodeGen.apply
        [ CodeGen.fqFun elementModule "html"
        , CodeGen.parens
            (CodeGen.apply
                [ CodeGen.fqFun htmlModule "br"
                , CodeGen.list []
                , CodeGen.list []
                ]
            )
        ]


emitText : Node -> TextData -> Expression
emitText node { text } =
    CodeGen.apply
        [ CodeGen.fqFun elementModule "el"
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , CodeGen.parens (CodeGen.apply [ CodeGen.fqFun elementModule "text", CodeGen.string text ])
        ]


emitButton : Node -> TextData -> Expression
emitButton node { text } =
    CodeGen.apply
        [ CodeGen.fqFun inputModule "button"
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , CodeGen.record
            [ ( "onPress", CodeGen.val "Nothing" )
            , ( "label", CodeGen.apply [ CodeGen.fqFun elementModule "text", CodeGen.string text ] )
            ]
        ]


emitCheckbox : Theme -> Node -> LabelData -> Expression
emitCheckbox theme node label =
    let
        labelPadding =
            { zero | left = Theme.xsmall theme }
    in
    CodeGen.apply
        [ CodeGen.fqFun inputModule "checkbox"
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , CodeGen.record
            [ ( "onChange", CodeGen.val "CheckboxClicked" )
            , ( "icon", CodeGen.fqFun inputModule "defaultCheckbox" )
            , ( "checked", CodeGen.val "False" )
            , ( "label"
              , CodeGen.apply
                    [ emitLabelPosition label.position
                    , CodeGen.list
                        []
                    , CodeGen.parens (CodeGen.apply [ CodeGen.fqFun elementModule "text", CodeGen.string label.text ])
                    ]
              )
            ]
        ]


emitTextField : Theme -> Node -> LabelData -> Expression
emitTextField theme node label =
    CodeGen.apply
        [ CodeGen.fqFun inputModule "text"
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , CodeGen.record
            [ ( "onChange", CodeGen.val "TextChanged" )
            , ( "text", CodeGen.string "" )
            , ( "placeholder", CodeGen.val "Nothing" )
            , ( "label"
              , CodeGen.apply
                    [ emitLabelPosition label.position
                    , CodeGen.list
                        [ CodeGen.apply [ CodeGen.fqFun fontModule "color", CodeGen.parens (emitColor theme.labelColor) ]
                        ]
                    , CodeGen.parens (CodeGen.apply [ CodeGen.fqFun elementModule "text", CodeGen.string label.text ])
                    ]
              )
            ]
        ]


emitTextFieldMultiline : Theme -> Node -> LabelData -> Expression
emitTextFieldMultiline theme node label =
    CodeGen.apply
        [ CodeGen.fqFun inputModule "multiline"
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , CodeGen.record
            [ ( "onChange", CodeGen.val "TextChanged" )
            , ( "text", CodeGen.string "" )
            , ( "placeholder", CodeGen.val "Nothing" )
            , ( "spellcheck", CodeGen.val "False" )
            , ( "label"
              , CodeGen.apply
                    [ emitLabelPosition label.position
                    , CodeGen.list
                        [ CodeGen.apply [ CodeGen.fqFun fontModule "color", CodeGen.parens (emitColor theme.labelColor) ]
                        ]
                    , CodeGen.parens (CodeGen.apply [ CodeGen.fqFun elementModule "text", CodeGen.string label.text ])
                    ]
              )
            ]
        ]


emitRadio : Theme -> Node -> LabelData -> List Expression -> Expression
emitRadio theme node label children =
    CodeGen.apply
        [ CodeGen.fqFun inputModule "radio"
        , CodeGen.list
            ([]
                |> emitAllStyles node
            )
        , CodeGen.record
            [ ( "onChange", CodeGen.val "RadioClicked" )
            , ( "selected", CodeGen.val "Nothing" )
            , ( "label"
              , CodeGen.apply
                    [ emitLabelPosition label.position
                    , CodeGen.list
                        [ CodeGen.apply [ CodeGen.fqFun fontModule "color", CodeGen.parens (emitColor theme.labelColor) ]
                        ]
                    , CodeGen.parens (CodeGen.apply [ CodeGen.fqFun elementModule "text", CodeGen.string label.text ])
                    ]
              )
            , ( "options", CodeGen.list children )
            ]
        ]


emitOption : Node -> TextData -> Expression
emitOption node { text } =
    CodeGen.apply
        [ CodeGen.fqFun inputModule "option"
        , CodeGen.int 1
        , CodeGen.parens
            (CodeGen.apply
                [ CodeGen.fqFun elementModule "el"
                , CodeGen.list
                    ([]
                        |> emitAllStyles node
                    )
                , CodeGen.parens (CodeGen.apply [ CodeGen.fqFun elementModule "text", CodeGen.string text ])
                ]
            )
        ]


emitImage : Node -> ImageData -> Expression
emitImage node image =
    CodeGen.apply
        [ CodeGen.fqFun elementModule "image"
        , CodeGen.list
            ([]
                |> clipIf (Border.isRounded node.borderCorner)
                |> emitAllStyles node
            )
        , CodeGen.record
            [ ( "src", CodeGen.string image.src )
            , ( "description", CodeGen.string image.description )
            ]
        ]


emitHeading : Node -> HeadingData -> Expression
emitHeading node { text, level } =
    -- Paragraph allows to wrap text and to set a line height
    CodeGen.apply
        [ CodeGen.fqFun elementModule "paragraph"
        , CodeGen.list
            ([ CodeGen.apply [ CodeGen.fqFun regionModule "heading", CodeGen.int level ]
             ]
                |> emitAllStyles node
            )
        , emitLines text
        ]



-- ATTRIBUTES


emitAllStyles : Node -> List Expression -> List Expression
emitAllStyles node attrs =
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
        |> emitBackground node.background


emitPadding : Padding -> List Expression -> List Expression
emitPadding value attrs =
    if value.top == 0 && value.right == 0 && value.bottom == 0 && value.left == 0 then
        attrs

    else if value.top == value.bottom && value.right == value.left then
        CodeGen.apply
            [ CodeGen.fqFun elementModule "paddingXY"
            , CodeGen.int value.right
            , CodeGen.int value.top
            ]
            :: attrs

    else
        CodeGen.apply
            [ CodeGen.fqFun elementModule "paddingEach"
            , CodeGen.record
                [ ( "top", CodeGen.int value.top )
                , ( "right", CodeGen.int value.right )
                , ( "bottom", CodeGen.int value.bottom )
                , ( "left", CodeGen.int value.left )
                ]
            ]
            :: attrs


emitBorder : Color -> BorderStyle -> BorderWidth -> List Expression -> List Expression
emitBorder borderColor borderStyle borderWidth attrs =
    if borderWidth.top == 0 && borderWidth.right == 0 && borderWidth.bottom == 0 && borderWidth.left == 0 then
        attrs

    else
        CodeGen.apply
            [ CodeGen.fqFun borderModule "color"
            , CodeGen.parens
                (emitColor borderColor)
            ]
            :: (case borderStyle of
                    Solid ->
                        CodeGen.fqFun borderModule "solid"

                    Dashed ->
                        CodeGen.fqFun borderModule "dashed"

                    Dotted ->
                        CodeGen.fqFun borderModule "dotted"
               )
            :: (if borderWidth.top == borderWidth.bottom && borderWidth.right == borderWidth.left then
                    CodeGen.apply
                        [ CodeGen.fqFun borderModule "widthXY"
                        , CodeGen.int borderWidth.right
                        , CodeGen.int borderWidth.top
                        ]

                else
                    CodeGen.apply
                        [ CodeGen.fqFun borderModule "widthEach"
                        , CodeGen.record
                            [ ( "top", CodeGen.int borderWidth.top )
                            , ( "right", CodeGen.int borderWidth.right )
                            , ( "bottom", CodeGen.int borderWidth.bottom )
                            , ( "left", CodeGen.int borderWidth.left )
                            ]
                        ]
               )
            :: attrs


emitCorner : BorderCorner -> List Expression -> List Expression
emitCorner borderCorner attrs =
    if borderCorner.topLeft == 0 && borderCorner.topRight == 0 && borderCorner.bottomLeft == 0 && borderCorner.bottomRight == 0 then
        attrs

    else if borderCorner.locked then
        CodeGen.apply
            [ CodeGen.fqFun borderModule "rounded"
            , CodeGen.int borderCorner.topLeft
            ]
            :: attrs

    else
        CodeGen.apply
            [ CodeGen.fqFun borderModule "roundEach"
            , CodeGen.record
                [ ( "topLeft", CodeGen.int borderCorner.topLeft )
                , ( "topRight", CodeGen.int borderCorner.topRight )
                , ( "bottomLeft", CodeGen.int borderCorner.bottomLeft )
                , ( "bottomRight", CodeGen.int borderCorner.bottomRight )
                ]
            ]
            :: attrs


emitColor value =
    let
        rgba =
            Element.toRgb value
    in
    CodeGen.apply
        [ CodeGen.fqFun elementModule "rgba255"
        , CodeGen.int <| round (rgba.red * 255)
        , CodeGen.int <| round (rgba.green * 255)
        , CodeGen.int <| round (rgba.blue * 255)
        , CodeGen.float rgba.alpha
        ]


emitFontFamily : Local FontFamily -> List Expression -> List Expression
emitFontFamily value attrs =
    case value of
        Local value_ ->
            case value_.type_ of
                Native fontStack ->
                    CodeGen.apply
                        [ CodeGen.fqFun fontModule "family"
                        , CodeGen.list
                            (List.map
                                (\name ->
                                    CodeGen.apply
                                        [ CodeGen.fqFun fontModule "typeface"
                                        , CodeGen.string name
                                        ]
                                )
                                fontStack
                            )
                        ]
                        :: attrs

                External url ->
                    CodeGen.apply
                        [ CodeGen.fqFun fontModule "family"
                        , CodeGen.list
                            [ CodeGen.apply
                                [ CodeGen.fqFun fontModule "typeface"
                                , CodeGen.string value_.name
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
            CodeGen.apply
                [ CodeGen.fqFun fontModule "color"
                , CodeGen.parens
                    (emitColor value_)
                ]
                :: attrs

        Inherit ->
            attrs


emitFontWeight : FontWeight -> List Expression -> List Expression
emitFontWeight value attrs =
    let
        emit_ w =
            CodeGen.apply
                [ CodeGen.fqFun fontModule w
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
            CodeGen.fqFun elementModule "spaceEvenly" :: attrs

        Spacing ( x, y ) ->
            if x == 0 && y == 0 then
                attrs

            else
                CodeGen.apply [ CodeGen.fqFun elementModule "spacingXY", CodeGen.int x, CodeGen.int y ] :: attrs


emitWidth : Length ->  Maybe Int -> Maybe Int -> List Expression -> List Expression
emitWidth value min max attrs =
    emitLength value min max (CodeGen.fqFun elementModule "width") attrs


emitHeight : Length -> Maybe Int -> Maybe Int -> List Expression -> List Expression
emitHeight value min max attrs =
    emitLength value min max (CodeGen.fqFun elementModule "height") attrs


emitLength : Length ->  Maybe Int -> Maybe Int -> Expression -> List Expression -> List Expression
emitLength value min max fun attrs =
    case value of
        Px px ->
            CodeGen.apply
                [ fun
                , CodeGen.parens
                    (CodeGen.pipe (CodeGen.apply [ CodeGen.fqFun elementModule "px", CodeGen.int px ])
                        ([]
                            |> emitMinLength min
                            |> emitMaxLength max
                        )
                    )
                ]
                :: attrs

        Content ->
            CodeGen.apply
                [ fun
                , CodeGen.parens
                    (CodeGen.pipe (CodeGen.fqFun elementModule "shrink")
                        ([]
                            |> emitMinLength min
                            |> emitMaxLength max
                        )
                    )
                ]
                :: attrs

        Fill portion ->
            (if portion == 1 then
                CodeGen.apply
                    [ fun
                    , CodeGen.parens
                        (CodeGen.pipe (CodeGen.fqFun elementModule "fill")
                            ([]
                                |> emitMinLength min
                                |> emitMaxLength max
                            )
                        )
                    ]

             else
                CodeGen.apply
                    [ fun
                    , CodeGen.parens
                        (CodeGen.pipe (CodeGen.apply [ CodeGen.fqFun elementModule "fillPortion", CodeGen.int portion ])
                            ([]
                                |> emitMinLength min
                                |> emitMaxLength max
                            )
                        )
                    ]
            )
                :: attrs

        Unspecified ->
            -- TODO Emit "shrink" as default to specify min/max values anyway?
            --   This is probably fixed in Elm UI 2 since we can emit min/max
            --   values indipendently of element width/height values
            attrs


emitMinLength : Maybe Int -> List Expression -> List Expression
emitMinLength value attrs =
    case value of
        Just value_ ->
            CodeGen.apply [ CodeGen.fqFun elementModule "minimum", CodeGen.int value_ ] :: attrs

        Nothing ->
            attrs


emitMaxLength : Maybe Int -> List Expression -> List Expression
emitMaxLength value attrs =
    case value of
        Just value_ ->
            CodeGen.apply [ CodeGen.fqFun elementModule "maximum", CodeGen.int value_ ] :: attrs

        Nothing ->
            attrs


emitAlignX : Alignment -> List Expression -> List Expression
emitAlignX value attrs =
    case value of
        Start ->
            CodeGen.fqFun elementModule "alignLeft" :: attrs

        Center ->
            CodeGen.fqFun elementModule "centerX" :: attrs

        End ->
            CodeGen.fqFun elementModule "alignRight" :: attrs

        None ->
            attrs


emitAlignY : Alignment -> List Expression -> List Expression
emitAlignY value attrs =
    case value of
        Start ->
            CodeGen.fqFun elementModule "alignTop" :: attrs

        Center ->
            CodeGen.fqFun elementModule "centerY" :: attrs

        End ->
            CodeGen.fqFun elementModule "alignBottom" :: attrs

        None ->
            attrs


emitFontSize : Local Int -> List Expression -> List Expression
emitFontSize value attrs =
    case value of
        Local size ->
            CodeGen.apply [ CodeGen.fqFun fontModule "size", CodeGen.int size ] :: attrs

        Inherit ->
            attrs


emitLetterSpacing : Float -> List Expression -> List Expression
emitLetterSpacing value attrs =
    if value /= 0 then
        CodeGen.apply [ CodeGen.fqFun fontModule "letterSpacing", CodeGen.float value ] :: attrs

    else
        attrs


emitWordSpacing : Float -> List Expression -> List Expression
emitWordSpacing value attrs =
    if value /= 0 then
        CodeGen.apply [ CodeGen.fqFun fontModule "wordSpacing", CodeGen.float value ] :: attrs

    else
        attrs


emitTextAlign : TextAlignment -> List Expression -> List Expression
emitTextAlign value attrs =
    case value of
        TextLeft ->
            --CodeGen.apply [ CodeGen.fqFun fontModule "alignLeft" ] :: attrs
            attrs

        TextCenter ->
            CodeGen.apply [ CodeGen.fqFun fontModule "center" ] :: attrs

        TextRight ->
            CodeGen.apply [ CodeGen.fqFun fontModule "alignRight" ] :: attrs

        TextJustify ->
            CodeGen.apply [ CodeGen.fqFun fontModule "justify" ] :: attrs


emitBackground : Background -> List Expression -> List Expression
emitBackground value attrs =
    case value of
        Background.Image value_ ->
            CodeGen.apply [ CodeGen.fqFun backgroundModule "image", CodeGen.string value_ ] :: attrs

        Background.Solid value_ ->
            CodeGen.apply
                [ CodeGen.fqFun backgroundModule "color"
                , CodeGen.parens
                    (emitColor value_)
                ]
                :: attrs

        Background.None ->
            attrs


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
        |> CodeGen.fqFun inputModule



-- HELPERS


clipIf pred attrs =
    if pred then
        CodeGen.fqFun elementModule "clip" :: attrs

    else
        attrs


zero =
    Layout.padding 0
