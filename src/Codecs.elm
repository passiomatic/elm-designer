module Codecs exposing
    ( encodeFontFamily
    , encodeFontWeight
    , encodeLabelPosition
    , encodePosition
    , encodeViewport
    , fontFamilyDecoder
    , fontWeightDecoder
    , fromString
    , labelPositionDecoder
    , positionDecoder
    , toString
    , viewportDecoder
    )

{-| Serialize and deserialize a document to JSON.
-}

import Codec exposing (Codec, Error, Value)
import Document exposing (..)
import Element exposing (Color, Orientation(..))
import Element.Border exposing (shadow)
import Element.Font as Font exposing (Font)
import Fonts
import Html.Events as E
import Json.Decode as D exposing (Decoder)
import Set exposing (Set)
import Style.Background as Background exposing (Background)
import Style.Border as Border exposing (..)
import Style.Font as Font exposing (..)
import Style.Input as Input exposing (LabelPosition(..))
import Style.Layout as Layout exposing (..)
import Style.Shadow as Shadow exposing (Shadow, ShadowType(..))
import Time exposing (Posix)
import Tree as T exposing (Tree)
import UUID


fromString : String -> Result Error Document
fromString value =
    Codec.decodeString documentCodec value


toString : Document -> String
toString value =
    Codec.encodeToString 0 documentCodec value


documentCodec : Codec Document
documentCodec =
    -- TODO Check schemaVersion and use oneOf to support multiple schemas
    Codec.object Document
        |> Codec.field "schemaVersion" .schemaVersion Codec.int
        |> Codec.field "lastUpdatedOn" .lastUpdatedOn timeCodec
        |> Codec.field "root" .root (treeCodec nodeCodec)
        |> Codec.field "selectedNodeId" .selectedNodeId nodeIdCodec
        |> Codec.field "viewport" .viewport viewportCodec
        |> Codec.field "collapsedTreeItems" .collapsedTreeItems setCodec
        |> Codec.buildObject


setCodec : Codec (Set String)
setCodec =
    Codec.map Set.fromList Set.toList (Codec.list Codec.string)


timeCodec : Codec Posix
timeCodec =
    Codec.map Time.millisToPosix Time.posixToMillis Codec.int


type alias Tree_ =
    { label : Node
    , nodes : List (Tree Node)
    }


treeCodec : Codec Node -> Codec (Tree Node)
treeCodec meta =
    Codec.recursive
        (\rmeta ->
            let
                toTree : Tree_ -> Tree Node
                toTree tree =
                    case tree.nodes of
                        [] ->
                            T.singleton tree.label

                        children ->
                            T.tree tree.label children

                fromTree tree =
                    Tree_ (T.label tree) (T.children tree)
            in
            Codec.object Tree_
                |> Codec.field "label" .label meta
                |> Codec.field "nodes" .nodes (Codec.list rmeta)
                |> Codec.buildObject
                |> Codec.map toTree fromTree
        )


nodeCodec : Codec Node
nodeCodec =
    Codec.object Node
        |> Codec.field "id" .id nodeIdCodec
        |> Codec.field "index" .index Codec.int
        |> Codec.field "name" .name Codec.string
        |> Codec.field "width" .width lengthCodec
        |> Codec.field "widthMin" .widthMin (Codec.maybe Codec.int)
        |> Codec.field "widthMax" .widthMax (Codec.maybe Codec.int)
        |> Codec.field "height" .height lengthCodec
        |> Codec.field "heightMin" .heightMin (Codec.maybe Codec.int)
        |> Codec.field "heightMax" .heightMax (Codec.maybe Codec.int)
        |> Codec.field "offsetX" .offsetX Codec.float
        |> Codec.field "offsetY" .offsetY Codec.float
        |> Codec.field "rotation" .rotation Codec.float
        |> Codec.field "scale" .scale Codec.float
        |> Codec.field "padding" .padding paddingCodec
        |> Codec.field "spacing" .spacing spacingCodec
        |> Codec.field "fontFamily" .fontFamily (localCodec fontFamilyCodec)
        |> Codec.field "fontColor" .fontColor (localCodec colorCodec)
        |> Codec.field "fontSize" .fontSize (localCodec Codec.int)
        |> Codec.field "fontWeight" .fontWeight fontWeightCodec
        |> Codec.field "letterSpacing" .letterSpacing Codec.float
        |> Codec.field "wordSpacing" .wordSpacing Codec.float
        |> Codec.field "textAlignment" .textAlignment textAlignmentCodec
        |> Codec.field "borderColor" .borderColor colorCodec
        |> Codec.field "borderStyle" .borderStyle borderStyleCodec
        |> Codec.field "borderWidth" .borderWidth borderWidthCodec
        |> Codec.field "borderCorner" .borderCorner borderCornerCodec
        |> Codec.field "shadow" .shadow shadowCodec
        |> Codec.field "background" .background backgroundCodec
        |> Codec.field "position" .position positionCodec
        |> Codec.field "alignmentX" .alignmentX alignmentCodec
        |> Codec.field "alignmentY" .alignmentY alignmentCodec
        |> Codec.field "type" .type_ nodeTypeCodec
        |> Codec.buildObject


nodeIdCodec : Codec NodeId
nodeIdCodec =
    Codec.string
        |> Codec.andThen
            (\value ->
                case UUID.fromString value of
                    Ok value_ ->
                        Codec.succeed value_

                    Err _ ->
                        Codec.fail "Failed to decode node UUID"
            )
            (\value ->
                UUID.toString value
            )


localCodec : Codec a -> Codec (Local a)
localCodec codec =
    Codec.custom
        (\local inherit value_ ->
            case value_ of
                Local value ->
                    local value

                Inherit ->
                    inherit
        )
        |> Codec.variant1 "Local" Local codec
        |> Codec.variant0 "Inherit" Inherit
        |> Codec.buildCustom


positionCodec : Codec Position
positionCodec =
    Codec.custom
        (\above below onStart onEnd inFront behindContent normal value_ ->
            case value_ of
                Above ->
                    above

                Below ->
                    below

                OnStart ->
                    onStart

                OnEnd ->
                    onEnd

                InFront ->
                    inFront

                BehindContent ->
                    behindContent

                Normal ->
                    normal
        )
        |> Codec.variant0 "Above" Above
        |> Codec.variant0 "Below" Below
        |> Codec.variant0 "OnStart" OnStart
        |> Codec.variant0 "OnEnd" OnEnd
        |> Codec.variant0 "InFront" InFront
        |> Codec.variant0 "BehindContent" BehindContent
        |> Codec.variant0 "Normal" Normal
        |> Codec.buildCustom


positionDecoder : (Position -> msg) -> Decoder msg
positionDecoder tagger =
    E.targetValue
        |> D.andThen (fromResult << Codec.decodeString positionCodec)
        |> D.map tagger


encodePosition : Position -> String
encodePosition value =
    Codec.encodeToString 0 positionCodec value


lengthCodec : Codec Length
lengthCodec =
    Codec.custom
        (\px content fill auto value_ ->
            case value_ of
                Px value ->
                    px value

                Content ->
                    content

                Fill value ->
                    fill value

                Unspecified ->
                    auto
        )
        |> Codec.variant1 "Px" Px Codec.int
        |> Codec.variant0 "Content" Content
        |> Codec.variant1 "Fill" Fill Codec.int
        |> Codec.variant0 "Unspecified" Unspecified
        |> Codec.buildCustom


paddingCodec : Codec Padding
paddingCodec =
    Codec.object Padding
        |> Codec.field "locked" .locked Codec.bool
        |> Codec.field "top" .top Codec.int
        |> Codec.field "right" .right Codec.int
        |> Codec.field "bottom" .bottom Codec.int
        |> Codec.field "left" .left Codec.int
        |> Codec.buildObject


spacingCodec : Codec Spacing
spacingCodec =
    Codec.custom
        (\spaceEvenly spacingXY value ->
            case value of
                SpaceEvenly ->
                    spaceEvenly

                Spacing ( x, y ) ->
                    spacingXY ( x, y )
        )
        |> Codec.variant0 "SpaceEvenly" SpaceEvenly
        |> Codec.variant1 "Spacing" Spacing (Codec.tuple Codec.int Codec.int)
        |> Codec.buildCustom



-- fontTypeCodec : Codec FontType
-- fontTypeCodec =
--     Codec.custom
--         (\native external value ->
--             case value of
--                 Native fonts ->
--                     native fonts
--                 External url ->
--                     external url
--         )
--         |> Codec.variant1 "Native" Native (Codec.list Codec.string)
--         |> Codec.variant1 "External" External Codec.string
--         |> Codec.buildCustom


{-| Unlike other styles we just need to serialize the font family name here.
-}
fontFamilyCodec : Codec FontFamily
fontFamilyCodec =
    Codec.string
        |> Codec.map
            (\name ->
                -- From JSON
                Fonts.findFamily name
            )
            (\family ->
                -- To JSON
                family.name
            )


encodeFontFamily : Local FontFamily -> String
encodeFontFamily value =
    Codec.encodeToString 0 (localCodec fontFamilyCodec) value


fontFamilyDecoder : (Local FontFamily -> msg) -> Decoder msg
fontFamilyDecoder tagger =
    E.targetValue
        |> D.andThen (fromResult << Codec.decodeString (localCodec fontFamilyCodec))
        |> D.map tagger


fontWeightCodec : Codec FontWeight
fontWeightCodec =
    Codec.custom
        (\heavy heavyItalic extraBold extraBoldItalic bold boldItalic semiBold semiBoldItalic medium mediumItalic regular italic light lightItalic extraLight extraLightItalic hairline hairlineItalic value_ ->
            case value_ of
                Heavy ->
                    heavy

                HeavyItalic ->
                    heavyItalic

                ExtraBold ->
                    extraBold

                ExtraBoldItalic ->
                    extraBoldItalic

                Bold ->
                    bold

                BoldItalic ->
                    boldItalic

                SemiBold ->
                    semiBold

                SemiBoldItalic ->
                    semiBoldItalic

                Medium ->
                    medium

                MediumItalic ->
                    mediumItalic

                Regular ->
                    regular

                Italic ->
                    italic

                Light ->
                    light

                LightItalic ->
                    lightItalic

                ExtraLight ->
                    extraLight

                ExtraLightItalic ->
                    extraLightItalic

                Hairline ->
                    hairline

                HairlineItalic ->
                    hairlineItalic
        )
        |> Codec.variant0 "Heavy" Heavy
        |> Codec.variant0 "HeavyItalic" HeavyItalic
        |> Codec.variant0 "ExtraBold" ExtraBold
        |> Codec.variant0 "ExtraBoldItalic" ExtraBoldItalic
        |> Codec.variant0 "Bold" Bold
        |> Codec.variant0 "BoldItalic" BoldItalic
        |> Codec.variant0 "SemiBold" SemiBold
        |> Codec.variant0 "SemiBoldItalic" SemiBoldItalic
        |> Codec.variant0 "Medium" Medium
        |> Codec.variant0 "MediumItalic" MediumItalic
        |> Codec.variant0 "Regular" Regular
        |> Codec.variant0 "Italic" Italic
        |> Codec.variant0 "Light" Light
        |> Codec.variant0 "LightItalic" LightItalic
        |> Codec.variant0 "ExtraLight" ExtraLight
        |> Codec.variant0 "ExtraLightItalic" ExtraLightItalic
        |> Codec.variant0 "Hairline" Hairline
        |> Codec.variant0 "HairlineItalic" HairlineItalic
        |> Codec.buildCustom


encodeFontWeight : FontWeight -> String
encodeFontWeight value =
    Codec.encodeToString 0 fontWeightCodec value


fontWeightDecoder : (FontWeight -> msg) -> Decoder msg
fontWeightDecoder tagger =
    E.targetValue
        |> D.andThen (fromResult << Codec.decodeString fontWeightCodec)
        |> D.map tagger


textAlignmentCodec : Codec TextAlignment
textAlignmentCodec =
    Codec.custom
        (\textStart textCenter textEnd textJustify value ->
            case value of
                TextStart ->
                    textStart

                TextCenter ->
                    textCenter

                TextEnd ->
                    textEnd

                TextJustify ->
                    textJustify
        )
        |> Codec.variant0 "TextStart" TextStart
        |> Codec.variant0 "TextCenter" TextCenter
        |> Codec.variant0 "TextEnd" TextEnd
        |> Codec.variant0 "TextJustify" TextJustify
        |> Codec.buildCustom


backgroundCodec : Codec Background
backgroundCodec =
    Codec.custom
        (\image solid none value ->
            case value of
                Background.Image value_ ->
                    image value_

                Background.Solid value_ ->
                    solid value_

                Background.None ->
                    none
        )
        |> Codec.variant1 "Image" Background.Image Codec.string
        |> Codec.variant1 "Solid" Background.Solid colorCodec
        |> Codec.variant0 "None" Background.None
        |> Codec.buildCustom


type alias Rgba =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


colorCodec : Codec Color
colorCodec =
    Codec.object Rgba
        |> Codec.field "red" .red Codec.float
        |> Codec.field "green" .green Codec.float
        |> Codec.field "blue" .blue Codec.float
        |> Codec.field "alpha" .alpha Codec.float
        |> Codec.buildObject
        |> Codec.map Element.fromRgb Element.toRgb


borderStyleCodec : Codec BorderStyle
borderStyleCodec =
    Codec.custom
        (\solid dashed dotted value ->
            case value of
                Solid ->
                    solid

                Dashed ->
                    dashed

                Dotted ->
                    dotted
        )
        |> Codec.variant0 "Solid" Solid
        |> Codec.variant0 "Dashed" Dashed
        |> Codec.variant0 "Dotted" Dotted
        |> Codec.buildCustom


borderCornerCodec : Codec BorderCorner
borderCornerCodec =
    Codec.object BorderCorner
        |> Codec.field "locked" .locked Codec.bool
        |> Codec.field "topLeft" .topLeft Codec.int
        |> Codec.field "topRight" .topRight Codec.int
        |> Codec.field "bottomRight" .bottomRight Codec.int
        |> Codec.field "bottomLeft" .bottomLeft Codec.int
        |> Codec.buildObject


borderWidthCodec : Codec BorderWidth
borderWidthCodec =
    Codec.object BorderWidth
        |> Codec.field "locked" .locked Codec.bool
        |> Codec.field "top" .top Codec.int
        |> Codec.field "right" .right Codec.int
        |> Codec.field "bottom" .bottom Codec.int
        |> Codec.field "left" .left Codec.int
        |> Codec.buildObject


shadowCodec : Codec Shadow
shadowCodec =
    Codec.object Shadow
        |> Codec.field "offsetX" .offsetX Codec.float
        |> Codec.field "offsetY" .offsetY Codec.float
        |> Codec.field "size" .size Codec.float
        |> Codec.field "blur" .blur Codec.float
        |> Codec.field "color" .color colorCodec
        |> Codec.field "type" .type_ shadowTypeCodec
        |> Codec.buildObject


shadowTypeCodec : Codec ShadowType
shadowTypeCodec =
    Codec.custom
        (\inner outer value ->
            case value of
                Inner ->
                    inner

                Outer ->
                    outer
        )
        |> Codec.variant0 "Inner" Inner
        |> Codec.variant0 "Outer" Outer
        |> Codec.buildCustom


nodeTypeCodec : Codec NodeType
nodeTypeCodec =
    Codec.custom
        (\headingNode paragraphNode textNode rowNode columnNode textColumnNode imageNode buttonNode checkboxNode textFieldNode textFieldMultilineNode radioNode optionNode pageNode documentNode value ->
            case value of
                DocumentNode ->
                    documentNode

                HeadingNode data ->
                    headingNode data

                ParagraphNode data ->
                    paragraphNode data

                TextNode data ->
                    textNode data

                RowNode data ->
                    rowNode data

                ColumnNode ->
                    columnNode

                TextColumnNode ->
                    textColumnNode

                ImageNode image ->
                    imageNode image

                ButtonNode data ->
                    buttonNode data

                CheckboxNode data ->
                    checkboxNode data

                TextFieldNode data ->
                    textFieldNode data

                TextFieldMultilineNode data ->
                    textFieldMultilineNode data

                RadioNode data ->
                    radioNode data

                OptionNode data ->
                    optionNode data

                PageNode ->
                    pageNode
        )
        |> Codec.variant1 "HeadingNode" HeadingNode headingCodec
        |> Codec.variant1 "ParagraphNode" ParagraphNode textCodec
        |> Codec.variant1 "TextNode" TextNode textCodec
        |> Codec.variant1 "RowNode" RowNode rowCodec
        |> Codec.variant0 "ColumnNode" ColumnNode
        |> Codec.variant0 "TextColumnNode" TextColumnNode
        |> Codec.variant1 "ImageNode" ImageNode imageCodec
        |> Codec.variant1 "ButtonNode" ButtonNode textCodec
        |> Codec.variant1 "CheckboxNode" CheckboxNode labelCodec
        |> Codec.variant1 "TextFieldNode" TextFieldNode labelCodec
        |> Codec.variant1 "TextFieldMultilineNode" TextFieldMultilineNode labelCodec
        |> Codec.variant1 "RadioNode" RadioNode labelCodec
        |> Codec.variant1 "OptionNode" OptionNode textCodec
        |> Codec.variant0 "PageNode" PageNode
        |> Codec.variant0 "DocumentNode" DocumentNode
        |> Codec.buildCustom


viewportCodec : Codec Viewport
viewportCodec =
    Codec.custom
        (\deviceModel custom fluid value ->
            case value of
                DeviceModel n ->
                    deviceModel n

                Custom w h o ->
                    custom w h o

                Fluid ->
                    fluid
        )
        |> Codec.variant1 "DeviceModel" DeviceModel Codec.string
        |> Codec.variant3 "Custom" Custom Codec.int Codec.int orientationCodec
        |> Codec.variant0 "Fluid" Fluid
        |> Codec.buildCustom


encodeViewport : Viewport -> String
encodeViewport value =
    Codec.encodeToString 0 viewportCodec value


viewportDecoder : (Viewport -> msg) -> Decoder msg
viewportDecoder tagger =
    E.targetValue
        |> D.andThen (fromResult << Codec.decodeString viewportCodec)
        |> D.map tagger


orientationCodec : Codec Orientation
orientationCodec =
    Codec.custom
        (\portrait landscape value ->
            case value of
                Portrait ->
                    portrait

                Landscape ->
                    landscape
        )
        |> Codec.variant0 "Portrait" Portrait
        |> Codec.variant0 "Landscape" Landscape
        |> Codec.buildCustom


labelCodec : Codec LabelData
labelCodec =
    Codec.object LabelData
        |> Codec.field "text" .text Codec.string
        |> Codec.field "position" .position labelPositionCodec
        |> Codec.field "color" .color (localCodec colorCodec)
        |> Codec.buildObject


labelPositionCodec : Codec LabelPosition
labelPositionCodec =
    Codec.custom
        (\above below left right hidden value ->
            case value of
                LabelAbove ->
                    above

                LabelBelow ->
                    below

                LabelLeft ->
                    left

                LabelRight ->
                    right

                LabelHidden ->
                    hidden
        )
        |> Codec.variant0 "LabelAbove" LabelAbove
        |> Codec.variant0 "LabelBelow" LabelBelow
        |> Codec.variant0 "LabelLeft" LabelLeft
        |> Codec.variant0 "LabelRight" LabelRight
        |> Codec.variant0 "LabelHidden" LabelHidden
        |> Codec.buildCustom


labelPositionDecoder : (LabelPosition -> msg) -> Decoder msg
labelPositionDecoder tagger =
    E.targetValue
        |> D.andThen (fromResult << Codec.decodeString labelPositionCodec)
        |> D.map tagger


encodeLabelPosition : LabelPosition -> String
encodeLabelPosition value =
    Codec.encodeToString 0 labelPositionCodec value


headingCodec : Codec HeadingData
headingCodec =
    Codec.object HeadingData
        |> Codec.field "text" .text Codec.string
        |> Codec.field "level" .level Codec.int
        |> Codec.buildObject


textCodec : Codec TextData
textCodec =
    Codec.object TextData
        |> Codec.field "text" .text Codec.string
        |> Codec.buildObject


rowCodec : Codec RowData
rowCodec =
    Codec.object RowData
        |> Codec.field "wrapped" .wrapped Codec.bool
        |> Codec.buildObject


imageCodec : Codec ImageData
imageCodec =
    Codec.object ImageData
        |> Codec.field "src" .src Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.buildObject



-- linkCodec : Codec Link
-- linkCodec =
--     Codec.object Link
--         |> Codec.field "url" .url Codec.string
--         |> Codec.field "label" .label Codec.string
--         |> Codec.buildObject


alignmentCodec : Codec Alignment
alignmentCodec =
    Codec.custom
        (\start center end none value ->
            case value of
                Start ->
                    start

                Center ->
                    center

                End ->
                    end

                None ->
                    none
        )
        |> Codec.variant0 "Start" Start
        |> Codec.variant0 "Center" Center
        |> Codec.variant0 "End" End
        |> Codec.variant0 "None" None
        |> Codec.buildCustom


fromResult : Result e a -> Decoder a
fromResult result =
    case result of
        Ok value ->
            D.succeed value

        Err _ ->
            D.fail "Cannot decode value object"
