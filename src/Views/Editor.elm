module Views.Editor exposing (view)

{-| Main view for the app.
-}

import Bootstrap.Tab as Tab
import CodeGen
import Codecs
import ContextMenu exposing (ContextMenu, Item)
import Css exposing (em, percent, px)
import Dict exposing (Dict)
import Document exposing (..)
import Element exposing (Color, Orientation(..))
import Element.Background exposing (image)
import File exposing (File)
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Entity as Entity
import Html.Events as E
import Html.Events.Extra.Wheel as Wheel
import Html5.DragDrop as DragDrop
import Icons
import Json.Decode as Decode exposing (Decoder)
import Library exposing (LibraryItem)
import Model exposing (..)
import Set exposing (Set)
import Style.Theme as Theme
import Tree as T exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UndoList
import Views.Common as Common exposing (none)
import Views.ContextMenuPopup as ContextMenuPopup
import Views.ElmUI as ElmUI
import Views.Inspector as Inspector


maxTreeLabelLength =
    50



-- VIEW


view model =
    H.node "main"
        [ A.classList
            [ ( "d-flex flex-column", True )
            , ( "dragging--element", Common.isDragging model.dragDrop )
            ]
        ]
        (case model.mode of
            PreviewMode ->
                [ headerView model
                , H.div [ A.class "d-flex" ]
                    [ workspaceView model
                    ]

                --, ContextMenuPopup.view model
                ]

            _ ->
                [ headerView model
                , H.div [ A.class "d-flex" ]
                    [ leftPaneView model
                    , workspaceView model
                    , rightPaneView model
                    ]
                , uploadProgressView model.uploadState
                , ContextMenuPopup.view model.contextMenu
                ]
        )


workspaceView model =
    -- let
    --     transformAttr =
    --         A.style "transform" (Css.scaleBy model.workspaceScale)
    --     topAttr =
    --         A.style "top" (px model.workspaceX)
    --     leftAttr =
    --         A.style "left" (px model.workspaceY)
    --     originX =
    --         Model.workspaceWidth // 2 - model.windowWidth // 2 + model.mouseX
    --     originY =
    --         Model.workspaceHeight // 2 - model.windowHeight // 2 + model.mouseY
    --     transformOriginAttr =
    --         A.style "transform-origin" (Css.px originX ++ " " ++ Css.px originY)
    -- in
    H.div
        [ A.class "workspace-wrapper flex-grow-1 unselectable"
        , A.id "workspace-wrapper"

        -- FIXME: Find a more descriptive way to pass isMetaDown information
        --, Wheel.onWithOptions { stopPropagation = True, preventDefault = model.isMetaDown } MouseWheelChanged
        ]
        [ H.div
            [ A.classList
                [ ( "workspace", True )
                , ( "workspace--design", model.mode == DesignMode )
                ]
            , A.style "width" (px Document.workspaceWidth)
            , A.style "height" (px Document.workspaceHeight)
            , A.id "workspace"
            ]
            [ documentView model
            ]
        ]


uploadProgressView uploadState =
    case uploadState of
        Uploading file _ sent ->
            let
                percent =
                    String.fromFloat (sent * 100)
            in
            H.div
                [ A.class "position-absolute w-100"
                , A.style "bottom" "1.4rem"
                , A.style "left" "0"
                , A.style "z-index" "3"
                ]
                [ H.div [ A.class "mx-auto bg-light border rounded bpx-2 bpy-2", A.style "max-width" "400px" ]
                    [ H.div [ A.class "label-sm" ] [ H.text ("Uploading image " ++ File.name file ++ "...") ]
                    , H.div [ A.class "mt-1 progress", A.style "height" "8px" ]
                        [ H.div
                            [ A.class "progress-bar progress-bar-striped progress-bar-animated"
                            , A.style "width" (percent ++ "%")
                            ]
                            []
                        ]
                    ]
                ]

        _ ->
            none


headerView : Model -> Html Msg
headerView model =
    let
        modeButton =
            case model.mode of
                DesignMode ->
                    H.button
                        ([ A.type_ "button"
                         , A.class "btn btn-secondary btn-sm"
                         , E.onClick (ModeChanged PreviewMode)
                         ]
                            |> Common.addTooltipDown "Start preview mode"
                        )
                        [ Icons.play ]

                PreviewMode ->
                    H.button
                        ([ A.type_ "button"
                         , A.class "btn btn-secondary btn-sm"
                         , E.onClick (ModeChanged DesignMode)
                         ]
                            |> Common.addTooltipDown "Stop preview mode"
                        )
                        [ Icons.stop ]
    in
    H.header [ A.class "header d-flex justify-content-between align-items-center bp-2 border-bottom", A.style "gap" "1rem" ]
        [ insertView model
        , undoRedoView model

        --, zoomView model
        --, modeButton
        ]


undoRedoView model =
    H.div [ A.class "me-auto btn-group" ]
        [ H.button
            ([ A.type_ "button"
             , A.class "btn btn-secondary btn-sm"
             , A.disabled (not (UndoList.hasPast model.document))
             , E.onClick Undo
             ]
                |> Common.addTooltipDown "Undo last change"
            )
            [ Icons.cornerUpLeft ]
        , H.button
            ([ A.type_ "button"
             , A.class "btn btn-secondary btn-sm"
             , A.disabled (not (UndoList.hasFuture model.document))
             , E.onClick Redo
             ]
                |> Common.addTooltipDown "Redo last change"
            )
            [ Icons.cornerUpRight ]
        ]


insertView model =
    let
        visible =
            case model.dropDownState of
                Visible id ->
                    id == InsertDropdown

                Hidden ->
                    False

        selectedNode =
            Zipper.label model.document.present
    in
    H.div
        [ A.class "dropdown"
        ]
        [ H.button
            [ A.class "btn btn-secondary btn-sm dropdown-toggle"
            , A.type_ "button"
            , E.onClick
                (DropDownChanged
                    (if visible then
                        Hidden

                     else
                        Visible InsertDropdown
                    )
                )
            ]
            [ H.text "Insert" ]
        , H.ul
            [ A.classList
                [ ( "dropdown-menu", True )
                , ( "show", visible )
                ]
            ]
            (insertImageView selectedNode
                :: dividerView
                :: List.map
                    (insertItemView selectedNode)
                    Library.items
            )
        ]


dividerView =
    H.li [] [ H.hr [ A.class "dropdown-divider" ] [] ]


insertImageView : Node -> Html Msg
insertImageView node =
    H.li []
        [ H.button
            [ A.classList
                [ ( "dropdown-item", True )
                , ( "disabled", not (Document.canInsertInto node Document.blankImageNode || Document.canInsertNextTo node Document.blankImageNode) )
                ]
            , A.type_ "button"
            , E.onClick InsertImageClicked
            ]
            [ H.text "Image..." ]
        ]


insertItemView : Node -> LibraryItem Msg -> Html Msg
insertItemView node item =
    let
        template =
            T.label item.root
    in
    H.li []
        [ H.button
            [ A.classList
                [ ( "dropdown-item", True )
                , ( "disabled", not (Document.canInsertInto node template.type_ || Document.canInsertNextTo node template.type_) )
                ]
            , A.type_ "button"
            , E.onClick (InsertNodeClicked item.root)
            ]
            [ H.text template.name ]
        ]



-- interactiveView : Model -> Html Msg
-- interactiveView model =
--     H.header [ A.class "header d-flex justify-content-center bpy-2 border-bottom" ]
--         [ viewportsView model
--         , H.div [ A.class "d-flex align-items-center" ]
--             [ zoomView model
--             , H.button [ A.type_ "button", A.class "btn btn-light btn-sm" ] [ Icons.play ]
--             ]
--         ]


viewportsView : Model -> Html Msg
viewportsView model =
    H.div [ A.class "d-flex align-items-center" ]
        [ H.text "Device"
        , H.select [ onViewportSelect ViewportChanged, A.class "form-select form-select-sm ms-1" ]
            (List.map
                (\viewport ->
                    let
                        setSelected other attrs =
                            A.selected (model.viewport == other) :: attrs

                        label =
                            case viewport of
                                Device name w h _ ->
                                    viewportLabel name w h

                                Custom w h _ ->
                                    viewportLabel "Custom" w h

                                Fluid ->
                                    "Fluid Layout"
                    in
                    H.option (setSelected viewport [ viewportValue viewport ])
                        [ H.text label ]
                )
                Document.viewports
            )
        ]


viewportLabel name width height =
    name
        ++ " "
        ++ Entity.mdash
        ++ " "
        ++ String.fromInt width
        ++ Entity.times
        ++ String.fromInt height
        ++ " px"


viewportValue : Viewport -> Attribute msg
viewportValue value =
    A.value (Codecs.encodeViewport value)


onViewportSelect msg =
    E.on "input" (Codecs.viewportDecoder msg)


zoomView : Model -> Html Msg
zoomView model =
    let
        zoom =
            round (model.workspaceScale * 100)
    in
    H.div [ A.class "d-flex bg-white border rounded align-items-center" ]
        [ H.button [ A.class "btn btn-transparent btn-sm" ] [ Icons.minusCircle ]
        , H.div [ A.class "text-center small", A.style "width" "3rem" ] [ H.text (String.fromInt zoom ++ "%") ]
        , H.button [ A.class "btn btn-transparent btn-sm" ] [ Icons.plusCircle ]
        ]


rightPaneView : Model -> Html Msg
rightPaneView model =
    H.aside [ A.class "pane pane--right border-start" ]
        [ Tab.config TabMsg
            |> Tab.attrs [ A.class "bpt-2" ]
            |> Tab.items
                [ Tab.item
                    { id = "tab-design"
                    , link = Tab.link [] [ H.text "Design" ]
                    , pane =
                        Tab.pane [ A.class "scroll-y" ]
                            (case Tab.activeTab model.rightPaneTabState of
                                Just "tab-design" ->
                                    Inspector.view model

                                _ ->
                                    []
                            )
                    }
                , Tab.item
                    { id = "tab-code"
                    , link = Tab.link [] [ H.text "Code" ]
                    , pane =
                        Tab.pane []
                            (case Tab.activeTab model.rightPaneTabState of
                                Just "tab-code" ->
                                    codeView model

                                _ ->
                                    []
                            )
                    }
                ]
            |> Tab.view model.rightPaneTabState
        ]


codeView : Model -> List (Html Msg)
codeView model =
    let
        tree =
            Zipper.tree model.document.present

        code =
            CodeGen.emit Theme.defaultTheme model.viewport tree

    in
    case (T.label tree).type_ of
        DocumentNode ->
            [ H.section [ A.class "section bp-3 d-flex flex-column align-items-center justify-content-center h-100" ]
                [ H.div [ A.class "text-muted" ]
                    [ H.text "Select a page or another element."
                    ]
                ]
            ]

        _ ->
            [ H.section [ A.class "section bp-3 d-flex flex-column h-100" ]
                [ H.div [ A.class "mb-2 fw-500" ]
                    [ H.text ("Generated code for " ++ (T.label tree |> .name))
                    ]
                , H.div [ A.class "scroll-y flex-fill bg-white bp-1 border" ]
                    [ H.pre [ A.class "preformatted" ]
                        [ H.text (CodeGen.emit Theme.defaultTheme model.viewport tree)
                        ]
                    ]
                , H.div [ A.class "mt-2 d-grid" ]
                    [ H.button [ E.onClick (ClipboardCopyClicked code), A.type_ "button", A.class "btn btn-primary" ]
                        [ H.text "Copy Elm code" ]
                    ]
                ]
            ]


leftPaneView : Model -> Html Msg
leftPaneView model =
    H.aside [ A.class "pane pane--left border-end d-flex flex-column" ]
        [ outlineView model
        , libraryView model
        ]


outlineView : Model -> Html Msg
outlineView model =
    let
        tree =
            Zipper.toTree model.document.present
    in
    H.div [ A.class "bp-3 scroll-y border-bottom flex-grow-1", A.style "flex-basis" "0" ]
        [ T.restructure identity (outlineItemView model) tree
        ]


outlineItemView : Model -> Node -> List (Html Msg) -> Html Msg
outlineItemView model node children =
    let
        collapsed =
            isCollapsed model node

        topHint =
            H.div
                (makeDroppableIf (Common.canDropNextTo node model.dragDrop)
                    (InsertBefore node.id)
                    [ A.classList
                        [ ( "tree__drop-hint tree__drop-hint--before", True )
                        , ( "tree__item--dropping", isDroppingBefore node.id model.dragDrop )
                        ]
                    ]
                )
                []

        bottomHint =
            H.div
                (makeDroppableIf (Common.canDropNextTo node model.dragDrop)
                    (InsertAfter node.id)
                    [ A.classList
                        [ ( "tree__drop-hint tree__drop-hint--after", True )
                        , ( "tree__item--dropping", isDroppingAfter node.id model.dragDrop )
                        ]
                    ]
                )
                []

        leafClasses =
            [ A.classList
                [ ( "tree__label", True )
                , ( "tree__label--leaf", True )
                , ( "bg-primary text-white", Document.isSelected node.id model.document.present )
                ]
            ]

        nodeClasses dropInto =
            [ A.classList
                [ ( "tree__label", True )
                , ( "tree__item--dropping", isDroppingInto node.id model.dragDrop )
                , ( "tree__item--can-drop", dropInto )
                , ( "bg-primary text-white", Document.isSelected node.id model.document.present )
                ]
            ]
    in
    case children of
        [] ->
            if Document.isDocumentNode node then
                -- Top node
                emptyDocumentNotice model node

            else
                H.li
                    [ A.class "position-relative" ]
                    [ topHint
                    , if Document.isContainer node then
                        let
                            dropInto =
                                Common.canDropInto node model.dragDrop
                        in
                        H.div
                            (nodeClasses dropInto
                                |> makeDroppableIf dropInto (AppendTo node.id)
                                |> makeDraggable (Move node)
                            )
                            (collapseIcon collapsed node [ treeLabel node ])

                      else
                        H.div
                            (leafClasses
                                |> makeDraggable (Move node)
                            )
                            [ treeLabel node ]
                    , bottomHint
                    ]

        _ ->
            if Document.isDocumentNode node then
                -- Top node
                H.div [ A.class "d-flex flex-column h-100" ]
                    [ H.div [ A.class "mb-2 fw-500" ]
                        [ H.text "Outline" ]
                    , H.ol
                        (A.classList
                            [ ( "tree rounded flex-grow-1", True )
                            , ( "tree--dropping", isDroppingInto node.id model.dragDrop )
                            ]
                            :: makeDroppableIf (Common.canDropInto node model.dragDrop) (AppendTo node.id) []
                        )
                        children
                    ]

            else
                H.li
                    [ A.classList
                        [ ( "position-relative", True )

                        -- Add a bit of padding so drop hints have room
                        --   while dealing with nested containers
                        , ( "bpb-1", not collapsed )
                        ]
                    ]
                    [ topHint
                    , H.div
                        (if Document.isContainer node then
                            let
                                dropInto =
                                    Common.canDropInto node model.dragDrop
                            in
                            nodeClasses dropInto
                                |> makeDroppableIf dropInto (AppendTo node.id)
                                |> makeDraggable (Move node)

                         else
                            leafClasses
                                |> makeDraggable (Move node)
                        )
                        (collapseIcon collapsed node [ treeLabel node ])
                    , if collapsed then
                        none

                      else
                        H.ol [ A.class "tree" ] children
                    , bottomHint
                    ]


emptyDocumentNotice model node =
    H.div
        (A.classList
            [ ( "d-flex flex-column border border-dashed justify-content-center rounded text-center text-muted h-100", True )
            , ( "tree--dropping", isDroppingInto node.id model.dragDrop )
            ]
            :: makeDroppableIf (Common.canDropInto node model.dragDrop) (AppendTo node.id) []
        )
        [ H.div [ A.class "large fw-bold" ] [ H.text "Workspace is empty" ]
        , H.text "Start adding pages."
        ]


treeLabel node =
    let
        label =
            (case node.type_ of
                ParagraphNode data ->
                    data.text

                HeadingNode data ->
                    data.text

                TextNode data ->
                    data.text

                ButtonNode data ->
                    data.text

                TextFieldNode data ->
                    data.text

                TextFieldMultilineNode data ->
                    data.text

                CheckboxNode data ->
                    data.text

                RadioNode data ->
                    data.text

                OptionNode data ->
                    data.text

                _ ->
                    ""
            )
                |> String.trim
                |> String.left maxTreeLabelLength
    in
    H.span
        [ A.class "w-100 text-truncate"
        , clickToSelectHandler node.id
        , ContextMenu.open ContextMenuMsg (OutlinePopup node.id)
        ]
        [ H.text
            (if String.isEmpty label then
                node.name

             else
                label
            )
        ]


collapseIcon collapsed node siblings =
    if collapsed then
        H.span [ clickToExpandHandler node.id ] [ Icons.chevronRight ]
            :: siblings

    else
        H.span [ clickToCollapseHandler node.id ] [ Icons.chevronDown ]
            :: siblings


isCollapsed model node =
    Set.member (Document.nodeId node.id) model.collapsedTreeItems


libraryView : Model -> Html Msg
libraryView _ =
    H.div [ A.class "bp-3 scroll-y flex-grow-1", A.style "flex-basis" "0" ]
        (H.div [ A.class "fw-500" ]
            [ H.text "Library" ]
            :: (Library.groups
                    |> List.map
                        (\( head, rest ) ->
                            H.section [ A.class "section mt-3" ]
                                [ H.h2 [ A.class "section__title mb-2" ]
                                    [ H.text head.group ]
                                , H.div [ A.class "library d-flex flex-wrap" ]
                                    (List.map libraryItemView (head :: rest))
                                ]
                        )
               )
        )


libraryItemView : LibraryItem Msg -> Html Msg
libraryItemView item =
    let
        template =
            T.label item.root
    in
    H.div
        (A.class "library__item bp-2 d-flex mb-1"
            :: DragDrop.draggable DragDropMsg (Insert item.root)
            |> Common.addTooltipDown item.description
        )
        [ H.span [ A.class "me-1" ]
            [ item.icon ]
        , H.div []
            [ H.text template.name ]
        ]


documentView : Model -> Html Msg
documentView model =
    let
        tree =
            Zipper.toTree model.document.present

        ctx =
            Model.context model

        ( viewportClass, width, height ) =
            case model.viewport of
                Device _ w h _ ->
                    ( "viewport--device", px w, px h )

                Custom w h _ ->
                    ( "viewport--custom", px w, px h )

                Fluid ->
                    ( "viewport--fluid", "calc(100% - 2px)", "calc(100% - 2px)" )

        content =
            ElmUI.render ctx tree
    in
    case model.mode of
        DesignMode ->
            content

        -- H.div
        --     [ A.classList
        --         [--( "page", True )
        --          --, ( "page--design", True )
        --          --, ( viewportClass, True )
        --         ]
        --     --, A.attribute "data-fold" height
        --     -- , A.style "width" width
        --     -- , A.style "min-height" height
        --     ]
        --     [ content
        --     -- , H.div
        --     --     [ A.class "page__fold"
        --     --     , A.style "top" height
        --     --     ]
        --     --     [ H.text "Fold" ]
        --     ]
        PreviewMode ->
            H.div
                [ A.classList
                    [ ( "chrome m-4", True )
                    , ( viewportClass, True )
                    ]
                ]
                [ H.div [ A.class "chrome__header d-flex justify-content-between" ]
                    [ H.div []
                        [ H.div [ A.class "chrome-button chrome-button--red me-2" ] [ H.text "" ]
                        , H.div [ A.class "chrome-button chrome-button--yellow me-2" ] [ H.text "" ]
                        , H.div [ A.class "chrome-button chrome-button--green" ] [ H.text "" ]
                        ]
                    , H.div [] [ H.text "Page Title" ]
                    , H.div [] [ H.text " " ]
                    ]
                , H.div
                    [ A.classList
                        [ ( "page", True )
                        , ( "page--interactive", True )
                        ]
                    , A.style "width" width
                    , A.style "height" height
                    ]
                    [ content
                    ]
                ]



-- HELPERS


preventDefaultOn : String -> Decoder msg -> Attribute msg
preventDefaultOn event decoder =
    E.preventDefaultOn event
        (Decode.map
            (\msg ->
                ( msg, True )
            )
            decoder
        )


clickToSelectHandler id =
    E.stopPropagationOn "click" (Decode.succeed ( NodeSelected False id, True ))


clickToExpandHandler =
    clickToCollapseHandler_ False


clickToCollapseHandler =
    clickToCollapseHandler_ True


clickToCollapseHandler_ collapse id =
    E.stopPropagationOn "click" (Decode.succeed ( CollapseNodeClicked collapse id, True ))


makeDroppable =
    makeDroppableIf True


makeDroppableIf pred dropId attrs =
    if pred then
        attrs ++ DragDrop.droppable DragDropMsg dropId

    else
        attrs


makeDraggable dragId attrs =
    attrs ++ DragDrop.draggable DragDropMsg dragId


isDroppingInto dropId dragDrop =
    case ( DragDrop.getDropId dragDrop, DragDrop.getDragId dragDrop ) of
        ( Just (AppendTo dropId_), Just (Move _) ) ->
            dropId_ == dropId

        ( Just (AppendTo dropId_), Just (Insert _) ) ->
            dropId_ == dropId

        ( Just _, _ ) ->
            -- We are dragging stuff in the workspace, no need to hilight anything
            False

        ( Nothing, _ ) ->
            False


isDroppingBefore dropId dragDrop =
    case DragDrop.getDropId dragDrop of
        Just (InsertBefore id) ->
            id == dropId

        _ ->
            False


isDroppingAfter dropId dragDrop =
    case DragDrop.getDropId dragDrop of
        Just (InsertAfter id) ->
            id == dropId

        _ ->
            False
