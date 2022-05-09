module Document exposing
    ( Document
    , DragId(..)
    , DropId(..)
    , HeadingData
    , ImageData
    , LabelData
    , Node
    , NodeId
    , NodeType(..)
    , RowData
    , TextData
    , Viewport(..)
    , appendNode
    , apply
    , applyAlign
    , applyAlignX
    , applyAlignY
    , applyBackground
    , applyBackgroundColor
    , applyBackgroundImage
    , applyBorder
    , applyBorderColor
    , applyBorderCorner
    , applyBorderLock
    , applyBorderStyle
    , applyBorderWidth
    , applyFontColor
    , applyFontFamily
    , applyFontSize
    , applyFontWeight
    , applyHeight
    , applyHeightMax
    , applyHeightMin
    , applyHeightWith
    , applyLabel
    , applyLabelColor
    , applyLabelPosition
    , applyLetterSpacing
    , applyOffset
    , applyPadding
    , applyPaddingLock
    , applyPosition
    , applyShadow
    , applyShadowColor
    , applyShadowFromString
    , applyShadowType
    , applySpacing
    , applyText
    , applyTextAlign
    , applyWidth
    , applyWidthMax
    , applyWidthMin
    , applyWidthWith
    , applyWordSpacing
    , applyWrapRowItems
    , baseTemplate
    , blankImageNode
    , canInsertInto
    , canInsertNextTo
    , createImageNode
    , defaultDevice
    , defaultDocument
    , devices
    , duplicateNode
    , emptyPage
    , fromTemplate
    , fromTemplateAt
    , generateId
    , getNextIndexFor
    , insertNode
    , insertNodeAfter
    , insertNodeBefore
    , isContainer
    , isDocumentNode
    , isImageNode
    , isPageNode
    , isSelected
    , nodeId
    , nodeType
    , removeNode
    , resolveInheritedFontColor
    , resolveInheritedFontFamily
    , resolveInheritedFontSize
    , schemaVersion
    , selectNodeWith
    , selectPageOf
    , viewports
    , workspaceHeight
    , workspaceWidth
    )

import Css
import Dict exposing (Dict)
import Element exposing (Color, Orientation(..))
import Fonts
import Maybe
import Palette
import Set exposing (Set)
import Style.Background as Background exposing (Background)
import Style.Border as Border exposing (BorderCorner, BorderStyle(..), BorderWidth)
import Style.Font as Font exposing (..)
import Style.Input as Input exposing (LabelPosition(..))
import Style.Layout as Layout exposing (..)
import Style.Shadow as Shadow exposing (Shadow, ShadowType)
import Style.Theme as Theme exposing (Theme)
import Time exposing (Posix)
import Tree as T exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UUID exposing (Seeds, UUID)


schemaVersion =
    4


{-| A serialized document.
-}
type alias Document =
    { schemaVersion : Int
    , lastUpdatedOn : Posix
    , root : Tree Node
    , selectedNodeId : NodeId
    , viewport : Viewport
    , collapsedTreeItems : Set String
    }


workspaceWidth =
    8000


workspaceHeight =
    8000


{-| UUID namespace for built-in library elements.
-}
defaultNamespace =
    UUID.forName "elm-designer.passiomatic.com" UUID.dnsNamespace


type alias NodeId =
    UUID


nodeId : NodeId -> String
nodeId value =
    UUID.toString value


type alias Node =
    { id : NodeId
    , index : Int
    , name : String
    , width : Length
    , widthMin : Maybe Int
    , widthMax : Maybe Int
    , height : Length
    , heightMin : Maybe Int
    , heightMax : Maybe Int
    , offsetX : Float
    , offsetY : Float
    , rotation : Float
    , scale : Float
    , padding : Padding
    , spacing : Spacing
    , fontFamily : Local FontFamily
    , fontColor : Local Color
    , fontSize : Local Int
    , fontWeight : FontWeight
    , letterSpacing : Float
    , wordSpacing : Float
    , textAlignment : TextAlignment
    , borderColor : Color
    , borderStyle : BorderStyle
    , borderWidth : BorderWidth
    , borderCorner : BorderCorner
    , shadow : Shadow
    , background : Background
    , position : Position
    , alignmentX : Alignment
    , alignmentY : Alignment
    , type_ : NodeType
    }


{-| What's being dragged.

    - Move is used to rearrage elements in the outlive view
    - Drag is used while dragging elements (only pages for now) on the workspace
    - Insert is used when dragging library elements into the workspace

-}
type DragId
    = Move Node
    | Drag Node
    | Insert (Tree Node)


type DropId
    = InsertAfter NodeId
    | InsertBefore NodeId
    | AppendTo NodeId


{-| Just-plain-boring template to build upon.
-}
baseTemplate : Node
baseTemplate =
    { name = ""
    , id = UUID.forName "node-element" defaultNamespace
    , index = 0
    , width = Layout.fit
    , widthMin = Nothing
    , widthMax = Nothing
    , height = Layout.fit
    , heightMin = Nothing
    , heightMax = Nothing
    , offsetX = 0
    , offsetY = 0
    , rotation = 0
    , scale = 1.0
    , padding = Layout.padding 0
    , spacing = Layout.spacing 0
    , fontFamily = Inherited
    , fontColor = Inherited
    , fontSize = Inherited
    , fontWeight = Regular
    , letterSpacing = 0
    , wordSpacing = 0
    , textAlignment = TextStart
    , borderColor = Palette.darkCharcoal
    , borderStyle = Solid
    , borderWidth = Border.width 0
    , borderCorner = Border.corner 0
    , shadow = Shadow.none
    , background = Background.None
    , position = Normal
    , alignmentX = None
    , alignmentY = None
    , type_ = PageNode
    }


type NodeType
    = HeadingNode HeadingData
    | ParagraphNode TextData
    | TextNode TextData
    | RowNode RowData
    | ColumnNode
    | TextColumnNode
    | ImageNode ImageData
    | ButtonNode TextData
    | CheckboxNode LabelData
    | TextFieldNode LabelData
    | TextFieldMultilineNode LabelData
    | RadioNode LabelData
    | OptionNode TextData
    | PageNode
    | DocumentNode


{-| UI name for node types.
-}
nodeType : NodeType -> String
nodeType value =
    case value of
        DocumentNode ->
            "Document"

        HeadingNode heading ->
            "Heading " ++ String.fromInt heading.level

        ParagraphNode _ ->
            "Paragraph"

        PageNode ->
            "Page"

        ColumnNode ->
            "Column"

        RowNode _ ->
            "Row"

        TextColumnNode ->
            "Text Column"

        ImageNode _ ->
            "Image"

        ButtonNode _ ->
            "Button"

        TextFieldNode _ ->
            "Text Field"

        TextFieldMultilineNode _ ->
            "Multiline Field"

        CheckboxNode _ ->
            "Checkbox"

        RadioNode _ ->
            "Radio Selection"

        OptionNode _ ->
            "Radio Option"

        TextNode _ ->
            "Text Snippet"


isPageNode : Node -> Bool
isPageNode node =
    case node.type_ of
        PageNode ->
            True

        _ ->
            False


isDocumentNode : Node -> Bool
isDocumentNode node =
    case node.type_ of
        DocumentNode ->
            True

        _ ->
            False


isImageNode : Node -> Bool
isImageNode node =
    case node.type_ of
        ImageNode _ ->
            True

        _ ->
            False


type alias TextData =
    { text : String
    }


type alias HeadingData =
    { text : String
    , level : Int
    }


type alias LabelData =
    { text : String
    , position : LabelPosition
    , color : Local Color
    }


type alias ImageData =
    { src : String
    , description : String
    , width : Maybe Int
    , height : Maybe Int
    , mimeType : Maybe String
    }


type alias RowData =
    { wrapped : Bool
    }



-- NODE CONSTRUCTION


generateId : Seeds -> ( NodeId, Seeds )
generateId seeds =
    UUID.step seeds


fromTemplateAt : { x : Float, y : Float } -> Tree Node -> Seeds -> (NodeType -> Int) -> ( Seeds, Tree Node )
fromTemplateAt position template seeds indexer =
    T.mapAccumulate
        (\seeds_ template_ ->
            let
                ( uuid, newSeeds ) =
                    generateId seeds_

                nextIndex =
                    indexer template_.type_

                newName =
                    template_.name ++ " " ++ String.fromInt nextIndex

                newNode =
                    case template_.type_ of
                        -- Always lay out pages absolutely within the workspace
                        PageNode ->
                            { template_
                                | id = uuid
                                , index = nextIndex
                                , name = newName
                                , offsetX = position.x
                                , offsetY = position.y
                            }

                        _ ->
                            { template_
                                | id = uuid
                                , index = nextIndex
                                , name = newName
                            }
            in
            ( newSeeds, newNode )
        )
        seeds
        template


fromTemplate : Tree Node -> Seeds -> (NodeType -> Int) -> ( Seeds, Tree Node )
fromTemplate template seeds indexer =
    fromTemplateAt { x = 0, y = 0 } template seeds indexer


{-| A startup document with a blank page on it.
-}
defaultDocument : Seeds -> ( Seeds, Tree Node )
defaultDocument seeds =
    let
        indexer _ =
            1

        template =
            T.tree
                { baseTemplate
                    | type_ = DocumentNode
                    , name = "Document"
                    , width = Layout.fill
                    , height = Layout.fill
                }
                [ -- TODO Pass actual theme value
                  emptyPage Theme.defaultTheme
                ]
    in
    fromTemplateAt { x = workspaceWidth / 2, y = workspaceHeight / 2 } template seeds indexer


emptyPage : Theme -> Tree Node
emptyPage theme =
    let
        ( width, height, _ ) =
            defaultDevice
    in
    T.singleton
        { baseTemplate
            | type_ = PageNode
            , name = "Page"
            , width = Layout.px width
            , height = Layout.unspecified
            , heightMin = Just height
            , fontFamily = Local theme.textFontFamily
            , fontColor = Local theme.textColor
            , fontSize = Local theme.textSize
            , position = InFront
            , background = Background.Solid theme.backgroundColor
        }


{-| Images require the user to drop them _into_ the app workspace so we bypass the pick-from-library process here.
-}
createImageNode : ImageData -> Seeds -> ( Seeds, Tree Node )
createImageNode data seeds =
    let
        -- TODO Generate correct index for images too
        indexer _ =
            1

        template =
            T.singleton
                { baseTemplate
                    | type_ = imageNode data
                    , name = "Image"
                    -- Make images fluid but do not overstretch them
                    , width = Layout.fill
                    , widthMax = data.width
                    , height = Layout.fill
                    , heightMax = data.height
                }
    in
    fromTemplate template seeds indexer


{-| An empty placeholder image type.
-}
blankImageNode =
    imageNode { src = "", description = "", width = Nothing, height = Nothing, mimeType = Nothing }


imageNode data =
    ImageNode data



-- VIEWPORTS


type Viewport
    = Device String Int Int Orientation
    | Custom Int Int Orientation
    | Fluid


devices =
    [ -- Android
      Device "Android" 360 640 Portrait
    , Device "Pixel 3" 411 823 Portrait
    , Device "Pixel 3 XL" 411 846 Portrait
    , Device "Pixel 4" 411 869 Portrait
    , Device "Pixel 4 XL" 411 869 Portrait
    , Device "Galaxy S10" 360 760 Portrait
    , Device "Galaxy S10+" 412 869 Portrait
    , Device "Galaxy S10 Lite" 412 914 Portrait

    -- Android tablet
    , Device "Nexus 7" 600 690 Portrait
    , Device "Nexus 9" 768 1024 Portrait
    , Device "Nexus 10" 800 1280 Portrait
    , Device "Pixel Slate" 1333 888 Portrait
    , Device "Pixelbook" 1200 800 Portrait

    -- Apple
    , Device "iPhone SE" 320 568 Portrait
    , Device "iPhone 8" 375 667 Portrait
    , Device "iPhone 8 Plus" 414 736 Portrait
    , Device "iPhone 11 Pro" 375 812 Portrait
    , Device "iPhone 11" 414 896 Portrait
    , Device "iPhone 11 Pro Max" 414 896 Portrait
    , Device "iPhone 12" 390 844 Portrait
    , Device "iPhone 12 Pro" 390 844 Portrait
    , Device "iPhone 12 Pro Max" 428 926 Portrait
    , Device "iPad mini 7.9\"" 768 1024 Portrait
    , Device "iPad 10.2\"" 810 1080 Portrait
    , Device "iPad Air 10.5\"" 834 1112 Portrait
    , Device "iPad Air 10.9\"" 840 1180 Portrait
    , Device "iPad Pro 11\"" 834 1194 Portrait
    , Device "iPad Pro 12.9\"" 1024 136 Portrait
    , Device "Apple TV" 1920 1080 Landscape

    -- Desktop
    , Device "Desktop" 1024 1024 Landscape
    , Device "Desktop HD" 1440 1024 Landscape
    ]


{-| Default is iPhone 8
-}
defaultDevice =
    ( 375, 667, Portrait )


viewports : List Viewport
viewports =
    Fluid :: devices



-- NODE QUERY


getNextIndexFor : NodeType -> Zipper Node -> Int
getNextIndexFor type_ zipper =
    T.foldl
        (\node accum ->
            if type_ == node.type_ && node.index >= accum then
                node.index + 1

            else
                accum
        )
        1
        (Zipper.tree zipper)


{-| Find the node with the given id and if successful move zipper focus to it.
-}
selectNodeWith : NodeId -> Zipper Node -> Maybe (Zipper Node)
selectNodeWith id zipper =
    Zipper.findFromRoot (\node -> node.id == id) zipper



{- Find the parent of the node with the given id and if successful move zipper focus to it. -}
-- selectParentOf : NodeId -> Zipper Node -> Maybe (Zipper Node)
-- selectParentOf id zipper =
--     selectNodeWith id zipper
--         |> Maybe.andThen Zipper.parent


{-| Find the page containing the node with the given id.
-}
selectPageOf : NodeId -> Zipper Node -> Maybe (Zipper Node)
selectPageOf id zipper =
    selectNodeWith id zipper
        |> selectPageOf_


selectPageOf_ : Maybe (Zipper Node) -> Maybe (Zipper Node)
selectPageOf_ maybeZipper =
    Maybe.andThen
        (\zipper ->
            let
                node =
                    Zipper.label zipper
            in
            case node.type_ of
                PageNode ->
                    Just zipper

                _ ->
                    selectPageOf_ (Zipper.parent zipper)
        )
        maybeZipper


resolveInheritedFontColor : Color -> Zipper Node -> Color
resolveInheritedFontColor default zipper =
    case resolveInheritedValue .fontColor (Just zipper) of
        Just value ->
            value

        Nothing ->
            default


resolveInheritedFontSize : Int -> Zipper Node -> Int
resolveInheritedFontSize default zipper =
    case resolveInheritedValue .fontSize (Just zipper) of
        Just value ->
            value

        Nothing ->
            default


resolveInheritedFontFamily : FontFamily -> Zipper Node -> FontFamily
resolveInheritedFontFamily default zipper =
    case resolveInheritedValue .fontFamily (Just zipper) of
        Just value ->
            value

        Nothing ->
            default


resolveInheritedValue : (Node -> Local a) -> Maybe (Zipper Node) -> Maybe a
resolveInheritedValue getter maybeZipper =
    case maybeZipper of
        Just zipper ->
            case getter (Zipper.label zipper) of
                Local value ->
                    Just value

                Inherited ->
                    resolveInheritedValue getter (Zipper.parent zipper)

        Nothing ->
            Nothing


isSelected : NodeId -> Zipper Node -> Bool
isSelected id zipper =
    let
        node =
            Zipper.label zipper
    in
    node.id == id


isContainer : Node -> Bool
isContainer node =
    case node.type_ of
        DocumentNode ->
            True

        PageNode ->
            True

        RowNode _ ->
            True

        ColumnNode ->
            True

        TextColumnNode ->
            True

        RadioNode _ ->
            True

        _ ->
            False


canInsertInto : Node -> NodeType -> Bool
canInsertInto node type_ =
    case ( node.type_, type_ ) of
        ( RadioNode _, OptionNode _ ) ->
            True

        ( _, OptionNode _ ) ->
            False

        ( PageNode, PageNode ) ->
            False

        ( PageNode, _ ) ->
            True

        ( DocumentNode, PageNode ) ->
            True

        ( _, PageNode ) ->
            False

        ( DocumentNode, _ ) ->
            False

        ( RowNode _, _ ) ->
            True

        ( ColumnNode, _ ) ->
            True

        ( TextColumnNode, _ ) ->
            True

        ( _, _ ) ->
            False


canInsertNextTo : Node -> NodeType -> Bool
canInsertNextTo node type_ =
    case ( node.type_, type_ ) of
        -- Only drop radio options next to another option
        ( OptionNode _, OptionNode _ ) ->
            True

        ( OptionNode _, _ ) ->
            False

        ( _, OptionNode _ ) ->
            False

        -- You cannot insert anything as document sibling
        ( DocumentNode, _ ) ->
            False

        -- Only drop pages next to another page
        ( PageNode, PageNode ) ->
            True

        ( PageNode, _ ) ->
            False

        -- Other scenarios
        ( _, RowNode _ ) ->
            True

        ( _, ColumnNode ) ->
            True

        ( _, TextColumnNode ) ->
            True

        ( _, ImageNode _ ) ->
            True

        ( _, HeadingNode _ ) ->
            True

        ( _, ParagraphNode _ ) ->
            True

        ( _, TextNode _ ) ->
            True

        ( _, ButtonNode _ ) ->
            True

        ( _, CheckboxNode _ ) ->
            True

        ( _, TextFieldNode _ ) ->
            True

        ( _, TextFieldMultilineNode _ ) ->
            True

        ( _, RadioNode _ ) ->
            True

        ( _, _ ) ->
            False



-- NODE EDIT


{-| Traverse the focussed node and generate a new node id for each children.
-}
duplicateNode : Zipper Node -> Seeds -> ( Seeds, Tree Node )
duplicateNode zipper seeds =
    Zipper.tree zipper
        |> T.mapAccumulate
            (\seeds_ node ->
                let
                    ( uuid, newSeeds ) =
                        generateId seeds_
                in
                ( newSeeds, { node | id = uuid } )
            )
            seeds


{-| Remove focussed node.
-}
removeNode : Zipper Node -> Zipper Node
removeNode zipper =
    Zipper.removeTree zipper
        |> Maybe.withDefault (Zipper.root zipper)


{-| A combined append/insert.
-}
insertNode : Tree Node -> Zipper Node -> Zipper Node
insertNode newTree zipper =
    let
        selectedNode =
            Zipper.label zipper
    in
    if canInsertInto selectedNode (T.label newTree).type_ then
        appendNode newTree zipper

    else
        let
            parentZipper =
                Zipper.parent zipper
                    |> Maybe.withDefault (Zipper.root zipper)
        in
        insertNodeAfter selectedNode.id newTree parentZipper


{-| Append the given node as children of focussed node and then move focus to it.
-}
appendNode : Tree Node -> Zipper Node -> Zipper Node
appendNode newTree zipper =
    Zipper.mapTree
        (T.appendChild newTree)
        zipper
        |> Zipper.lastChild
        -- Handle degenerate case
        |> Maybe.withDefault (Zipper.root zipper)


{-| Insert the given node after its sibling, or zipper root as fallback, and move focus to it.
-}
insertNodeAfter : NodeId -> Tree Node -> Zipper Node -> Zipper Node
insertNodeAfter siblingId newTree zipper =
    selectNodeWith siblingId zipper
        |> Maybe.map (Zipper.append newTree)
        |> Maybe.andThen Zipper.nextSibling
        -- Handle degenerate case
        |> Maybe.withDefault (Zipper.root zipper)


{-| Insert the given node before its sibling, or zipper root as fallback, and move focus to it.
-}
insertNodeBefore : NodeId -> Tree Node -> Zipper Node -> Zipper Node
insertNodeBefore siblingId newTree zipper =
    selectNodeWith siblingId zipper
        |> Maybe.map (Zipper.prepend newTree)
        |> Maybe.andThen Zipper.previousSibling
        -- Handle degenerate case
        |> Maybe.withDefault (Zipper.root zipper)



-- NODE PROPERTIES


setText : String -> { a | text : String } -> { a | text : String }
setText value record =
    { record | text = value }


{-| Apply a function to the focussed node.
-}
apply : (Node -> Node) -> Zipper Node -> Zipper Node
apply setter zipper =
    Zipper.mapLabel setter zipper


applyLabel : String -> Zipper Node -> Zipper Node
applyLabel value zipper =
    let
        value_ =
            String.trim value
    in
    Zipper.mapLabel
        (\node ->
            case node.type_ of
                TextFieldNode label ->
                    { node | type_ = TextFieldNode (setText value_ label) }

                TextFieldMultilineNode label ->
                    { node | type_ = TextFieldMultilineNode (setText value_ label) }

                ButtonNode button ->
                    { node | type_ = ButtonNode (setText value_ button) }

                CheckboxNode label ->
                    { node | type_ = CheckboxNode (setText value_ label) }

                RadioNode label ->
                    { node | type_ = RadioNode (setText value_ label) }

                OptionNode label ->
                    { node | type_ = OptionNode (setText value_ label) }

                _ ->
                    node
        )
        zipper


applyLabelPosition : LabelPosition -> Zipper Node -> Zipper Node
applyLabelPosition value zipper =
    Zipper.mapLabel
        (\node ->
            case node.type_ of
                TextFieldNode data ->
                    { node | type_ = TextFieldNode (Input.setLabelPosition value data) }

                TextFieldMultilineNode data ->
                    { node | type_ = TextFieldMultilineNode (Input.setLabelPosition value data) }

                CheckboxNode data ->
                    { node | type_ = CheckboxNode (Input.setLabelPosition value data) }

                RadioNode data ->
                    { node | type_ = RadioNode (Input.setLabelPosition value data) }

                _ ->
                    node
        )
        zipper


applyLabelColor : String -> Zipper Node -> Zipper Node
applyLabelColor value zipper =
    let
        value_ =
            Local (Css.stringToColor value)
    in
    Zipper.mapLabel
        (\node ->
            case node.type_ of
                TextFieldNode data ->
                    { node | type_ = TextFieldNode (Input.setLabelColor value_ data) }

                TextFieldMultilineNode data ->
                    { node | type_ = TextFieldMultilineNode (Input.setLabelColor value_ data) }

                CheckboxNode data ->
                    { node | type_ = CheckboxNode (Input.setLabelColor value_ data) }

                RadioNode data ->
                    { node | type_ = RadioNode (Input.setLabelColor value_ data) }

                _ ->
                    node
        )
        zipper



-- applyImageSrc : String -> Zipper Node -> Zipper Node
-- applyImageSrc value zipper =
--     Zipper.mapLabel
--         (\node ->
--             case node.type_ of
--                 ImageNode image ->
--                     { node | type_ = ImageNode { image | src = value } }
--                 _ ->
--                     node
--         )
--         zipper


applyText : String -> Zipper Node -> Zipper Node
applyText value zipper =
    Zipper.mapLabel
        (\node ->
            case node.type_ of
                ParagraphNode data ->
                    { node | type_ = ParagraphNode (setText value data) }

                HeadingNode data ->
                    { node | type_ = HeadingNode (setText value data) }

                TextNode data ->
                    { node | type_ = TextNode (setText value data) }

                _ ->
                    node
        )
        zipper


applyTextAlign : TextAlignment -> Zipper Node -> Zipper Node
applyTextAlign value zipper =
    Zipper.mapLabel (Font.setTextAlignment value) zipper


applyWrapRowItems : Bool -> Zipper Node -> Zipper Node
applyWrapRowItems value zipper =
    Zipper.mapLabel (\node -> { node | type_ = RowNode { wrapped = value } }) zipper


applySpacing : (Int -> Spacing -> Spacing) -> String -> Zipper Node -> Zipper Node
applySpacing setter value zipper =
    let
        value_ =
            String.toInt value
                |> Maybe.map (clamp 0 999)
                |> Maybe.withDefault 0
    in
    Zipper.mapLabel (\node -> Layout.setSpacing (setter value_ node.spacing) node) zipper


applyPadding : (Int -> Padding -> Padding) -> String -> Zipper Node -> Zipper Node
applyPadding setter value zipper =
    let
        value_ =
            String.toInt value
                |> Maybe.map (clamp 0 999)
                |> Maybe.withDefault 0
    in
    Zipper.mapLabel (\node -> Layout.setPadding (setter value_ node.padding) node) zipper


applyPaddingLock : Bool -> Zipper Node -> Zipper Node
applyPaddingLock value zipper =
    Zipper.mapLabel (\node -> Layout.setPadding (setLock value node.padding) node) zipper


applyBorderLock : Bool -> Zipper Node -> Zipper Node
applyBorderLock value zipper =
    -- Set both values together
    Zipper.mapLabel
        (\node ->
            node
                |> Border.setWidth (setLock value node.borderWidth)
                |> Border.setCorner (setLock value node.borderCorner)
        )
        zipper


applyAlignX : Alignment -> Zipper Node -> Zipper Node
applyAlignX value zipper =
    Zipper.mapLabel (\node -> { node | alignmentX = value }) zipper


applyAlignY : Alignment -> Zipper Node -> Zipper Node
applyAlignY value zipper =
    Zipper.mapLabel (\node -> { node | alignmentY = value }) zipper


applyAlign : Alignment -> Zipper Node -> Zipper Node
applyAlign value zipper =
    Zipper.mapLabel
        (\node ->
            { node | alignmentX = value, alignmentY = value }
        )
        zipper


applyOffset : (Float -> Node -> Node) -> String -> Zipper Node -> Zipper Node
applyOffset setter value zipper =
    let
        value_ =
            String.toFloat value
                |> Maybe.map (clamp -999 999)
                |> Maybe.withDefault 0
    in
    Zipper.mapLabel (setter value_) zipper


applyPosition : Position -> Zipper Node -> Zipper Node
applyPosition value zipper =
    Zipper.mapLabel (Layout.setPosition value) zipper


applyWidth : Length -> Zipper Node -> Zipper Node
applyWidth value zipper =
    Zipper.mapLabel (\node -> { node | width = value }) zipper


applyWidthWith : (Maybe Int -> Length -> Length) -> String -> Zipper Node -> Zipper Node
applyWidthWith setter value zipper =
    let
        value_ =
            String.toInt value
                |> Maybe.map (clamp 0 9999)
    in
    Zipper.mapLabel (\node -> { node | width = setter value_ node.width }) zipper


applyWidthMin : String -> Zipper Node -> Zipper Node
applyWidthMin value zipper =
    let
        value_ =
            String.toInt value
                |> Maybe.map (clamp 0 9999)
    in
    Zipper.mapLabel (\node -> { node | widthMin = value_ }) zipper


applyWidthMax : String -> Zipper Node -> Zipper Node
applyWidthMax value zipper =
    let
        value_ =
            String.toInt value
                |> Maybe.map (clamp 0 9999)
    in
    Zipper.mapLabel (\node -> { node | widthMax = value_ }) zipper


applyHeight : Length -> Zipper Node -> Zipper Node
applyHeight value zipper =
    Zipper.mapLabel (\node -> { node | height = value }) zipper


applyHeightWith : (Maybe Int -> Length -> Length) -> String -> Zipper Node -> Zipper Node
applyHeightWith setter value zipper =
    let
        value_ =
            String.toInt value
                |> Maybe.map (clamp 0 9999)
    in
    Zipper.mapLabel (\node -> { node | height = setter value_ node.height }) zipper


applyHeightMin : String -> Zipper Node -> Zipper Node
applyHeightMin value zipper =
    let
        value_ =
            String.toInt value
                |> Maybe.map (clamp 0 9999)
    in
    Zipper.mapLabel (\node -> { node | heightMin = value_ }) zipper


applyHeightMax : String -> Zipper Node -> Zipper Node
applyHeightMax value zipper =
    let
        value_ =
            String.toInt value
                |> Maybe.map (clamp 0 9999)
    in
    Zipper.mapLabel (\node -> { node | heightMax = value_ }) zipper


applyFontSize : String -> Zipper Node -> Zipper Node
applyFontSize value zipper =
    let
        value_ =
            case String.toInt value of
                Just v ->
                    Local (clamp Font.minFontSizeAllowed 999 v)

                Nothing ->
                    Inherited
    in
    Zipper.mapLabel (Font.setSize value_) zipper


applyFontFamily : Local FontFamily -> Zipper Node -> Zipper Node
applyFontFamily value zipper =
    let
        -- First, apply the new family so the inheritance chain is consistent
        newZipper =
            Zipper.mapLabel (Font.setFamily value) zipper
    in
    Zipper.mapLabel
        (\node ->
            let
                resolvedFamily =
                    resolveInheritedFontFamily Fonts.defaultFamily newZipper

                -- While changing family adjust weight to the closest available
                newWeight =
                    Font.findClosestWeight node.fontWeight resolvedFamily.weights
            in
            node
                |> Font.setWeight newWeight
        )
        newZipper


applyLetterSpacing : String -> Zipper Node -> Zipper Node
applyLetterSpacing value zipper =
    let
        value_ =
            String.toFloat value
                |> Maybe.map (clamp -999 999)
                |> Maybe.withDefault 0
    in
    Zipper.mapLabel (Font.setLetterSpacing value_) zipper


applyWordSpacing : String -> Zipper Node -> Zipper Node
applyWordSpacing value zipper =
    let
        value_ =
            String.toFloat value
                |> Maybe.map (clamp -999 999)
                |> Maybe.withDefault 0
    in
    Zipper.mapLabel (Font.setWordSpacing value_) zipper


applyBackgroundColor : String -> Zipper Node -> Zipper Node
applyBackgroundColor value zipper =
    let
        value_ =
            if String.trim value /= "" then
                Background.Solid (Css.stringToColor value)

            else
                Background.None
    in
    Zipper.mapLabel (Background.setBackground value_) zipper


applyBackgroundImage : String -> Zipper Node -> Zipper Node
applyBackgroundImage value zipper =
    let
        value_ =
            if String.trim value /= "" then
                Background.Image value

            else
                Background.None
    in
    Zipper.mapLabel (Background.setBackground value_) zipper


applyBackground : Background -> Zipper Node -> Zipper Node
applyBackground value zipper =
    Zipper.mapLabel (Background.setBackground value) zipper


applyBorderColor : String -> Zipper Node -> Zipper Node
applyBorderColor value zipper =
    let
        value_ =
            Css.stringToColor value
    in
    Zipper.mapLabel (Border.setColor value_) zipper


applyBorder : BorderWidth -> Zipper Node -> Zipper Node
applyBorder width zipper =
    -- @@TODO Merge width and corner into a single record to handle this better
    Zipper.mapLabel (Border.setWidth width >> Border.setCorner (Border.corner 0)) zipper


applyBorderWidth : (Int -> BorderWidth -> BorderWidth) -> String -> Zipper Node -> Zipper Node
applyBorderWidth setter value zipper =
    let
        value_ =
            String.toInt value
                |> Maybe.map (clamp 0 999)
                |> Maybe.withDefault 0
    in
    Zipper.mapLabel (\node -> Border.setWidth (setter value_ node.borderWidth) node) zipper


applyBorderStyle : BorderStyle -> Zipper Node -> Zipper Node
applyBorderStyle value zipper =
    Zipper.mapLabel (Border.setStyle value) zipper


applyBorderCorner : (Int -> BorderCorner -> BorderCorner) -> String -> Zipper Node -> Zipper Node
applyBorderCorner setter value zipper =
    let
        value_ =
            String.toInt value
                |> Maybe.map (clamp 0 999)
                |> Maybe.withDefault 0
    in
    Zipper.mapLabel (\node -> Border.setCorner (setter value_ node.borderCorner) node) zipper


applyFontColor : String -> Zipper Node -> Zipper Node
applyFontColor value zipper =
    let
        value_ =
            Local (Css.stringToColor value)
    in
    Zipper.mapLabel (Font.setColor value_) zipper


applyFontWeight : FontWeight -> Zipper Node -> Zipper Node
applyFontWeight value zipper =
    Zipper.mapLabel (Font.setWeight value) zipper


applyShadowFromString : (Float -> Shadow -> Shadow) -> String -> Zipper Node -> Zipper Node
applyShadowFromString setter value zipper =
    let
        value_ =
            String.toFloat value
                -- TODO handle negative and positive offset values whle clamping 0-positive blur and size
                --|> Maybe.map (clamp 0 999)
                |> Maybe.withDefault 0
    in
    Zipper.mapLabel (\node -> Shadow.setShadow (setter value_ node.shadow) node) zipper


applyShadow : Shadow -> Zipper Node -> Zipper Node
applyShadow value zipper =
    Zipper.mapLabel (\node -> Shadow.setShadow value node) zipper


applyShadowColor : String -> Zipper Node -> Zipper Node
applyShadowColor value zipper =
    let
        value_ =
            Css.stringToColor value
    in
    Zipper.mapLabel (\node -> Shadow.setShadow (Shadow.setColor value_ node.shadow) node) zipper


applyShadowType : ShadowType -> Zipper Node -> Zipper Node
applyShadowType value zipper =
    Zipper.mapLabel (\node -> Shadow.setShadow (Shadow.setType value node.shadow) node) zipper
