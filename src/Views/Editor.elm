module Views.Editor exposing (view)

{-| Main view for the app.
-}

import Array
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
import Html5.DragDrop as DragDrop
import Icons
import Json.Decode as Decode exposing (Decoder)
import Library exposing (LibraryItem)
import Model exposing (..)
import Palette
import SelectList exposing (SelectList)
import Set exposing (Set)
import Style.Theme as Theme
import Tree as T exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import Uploader
import Views.Common as Common exposing (none)
import Views.ContextMenus as ContextMenus
import Views.ElmUI as ElmUI
import Views.Inspector as Inspector


maxTreeLabelLength =
    50



-- VIEW


view model =
    H.node "main"
        [ A.classList
            [ ( "h-100", True )
            , ( "dragging--element", Common.isDragging model.dragDrop )
            ]
        ]
        (case model.mode of
            PreviewMode ->
                [ headerView model
                , H.div [ A.class "d-flex" ]
                    [ workspaceView model
                    ]
                , ContextMenus.pageListView model
                ]

            _ ->
                [ headerView model
                , H.div [ A.class "d-flex" ]
                    [ leftPaneView model
                    , workspaceView model
                    , rightPaneView model
                    ]
                , ContextMenus.pageListView model
                ]
        )


workspaceView model =
    H.div
        [ A.classList
            [ ( "workspace flex-grow-1 unselectable", True )
            , ( "workspace--design", model.mode == DesignMode )
            , ( "workspace--preview", model.mode == PreviewMode )
            ]
        ]
        [ pageView model
        , uploadProgressView model.uploadState

        --, uploadFileView
        ]


{-| This is hidden in the UI but needed to trigget the "Pick a file..." native dialog
-}



-- uploadFileView =
--     H.input
--         [ A.type_ "file"
--         , A.multiple True
--         , E.on "change" (Decode.map FileSelected filesDecoder)
--         ]
--         []
-- filesDecoder : Decoder (List File)
-- filesDecoder =
--     Decode.at [ "target", "files" ] (Decode.list File.decoder)


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
                [ H.div [ A.class "w-50 mx-auto bg-light border rounded bpx-2 bpy-2" ]
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
                        [ A.type_ "button"
                        , A.class "btn btn-light btn-sm"
                        , A.title "Start preview mode (P key)"
                        , E.onClick (ModeChanged PreviewMode)
                        ]
                        [ Icons.play ]

                PreviewMode ->
                    H.button
                        [ A.type_ "button"
                        , A.class "btn btn-light btn-sm"
                        , A.title "Stop preview mode (Esc key)"
                        , E.onClick (ModeChanged DesignMode)
                        ]
                        [ Icons.stop ]
    in
    H.header [ A.class "header d-flex justify-content-between align-items-center bp-2 border-bottom" ]
        [ insertView model
        , viewportsView model

        --, modeButton
        , none
        ]


insertView model =
    let
        visible =
            case model.dropDownState of
                Visible id ->
                    id == InsertDropdown

                Hidden ->
                    False
    in
    H.div
        [ A.class "dropdown"
        ]
        [ H.button
            [ A.class "btn btn-light btn-sm dropdown-toggle"
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
            (insertImageView
                :: dividerView
                :: List.map
                    insertItemView
                    Library.items
            )
        ]


dividerView =
    H.li [] [ H.hr [ A.class "dropdown-divider" ] [] ]


insertImageView =
    H.li []
        [ H.button
            [ A.class "dropdown-item"
            , A.type_ "button"
            , E.onClick InsertImageClicked
            ]
            [ H.text "Image..." ]
        ]


insertItemView item =
    let
        template =
            T.label item.root
    in
    H.li []
        [ H.button
            [ A.class "dropdown-item"
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
                                DeviceModel name ->
                                    let
                                        ( w, h, _ ) =
                                            Document.findDeviceInfo name
                                    in
                                    name
                                        ++ " "
                                        ++ Entity.mdash
                                        ++ " "
                                        ++ String.fromInt w
                                        ++ Entity.times
                                        ++ String.fromInt h
                                        ++ " px"

                                Custom w h _ ->
                                    "Custom"
                                        ++ " "
                                        ++ Entity.mdash
                                        ++ " "
                                        ++ String.fromInt w
                                        ++ Entity.times
                                        ++ String.fromInt h
                                        ++ " px"

                                Fluid ->
                                    "Fluid Layout"
                    in
                    H.option (setSelected viewport [ viewportValue viewport ])
                        [ H.text label ]
                )
                Document.viewports
            )
        ]


viewportValue : Viewport -> Attribute msg
viewportValue value =
    A.value (Codecs.encodeViewport value)


onViewportSelect msg =
    --E.stopPropagationOn "input" (Codecs.viewportDecoder msg)
    E.on "input" (Codecs.viewportDecoder msg)



-- zoomView : Model -> Html Msg
-- zoomView model =
--     let
--         zoom =
--             round (model.workspaceScale * 100)
--     in
--     H.div [ A.class "d-flex align-items-center me-5" ]
--         [ Button.button [ Button.light, Button.small ] [ Icons.minusCircle ]
--         , H.div [ A.class "bp-3 bg-white rounded text-center", A.style "width" "3rem" ] [ H.text (String.fromInt zoom ++ "%") ]
--         , Button.button [ Button.light, Button.small ] [ Icons.plusCircle ]
--         ]


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
        node =
            page model.pages.present
                |> Zipper.tree
    in
    [ H.section [ A.class "section bp-3 d-flex flex-column h-100" ]
        [ H.div [ A.class "mb-2 fw-500" ]
            [ H.text ("Generated code for " ++ (T.label node |> .name))
            ]
        , H.div [ A.class "scroll-y flex-fill bg-white bp-1 border" ]
            [ H.pre [ A.class "preformatted" ]
                [ H.text (CodeGen.emit Theme.defaultTheme model.viewport node)
                ]
            ]
        , H.div [ A.class "mt-2" ]
            [ H.button [ E.onClick ClipboardCopyClicked, A.type_ "button", A.class "btn btn-primary btn-block" ]
                [ H.text "Copy Elm code" ]
            ]
        ]
    ]


leftPaneView : Model -> Html Msg
leftPaneView model =
    H.aside [ A.class "pane pane--left border-end d-flex flex-column" ]
        [ pageListView model
        , outlineView model
        , libraryView model
        ]


outlineView : Model -> Html Msg
outlineView model =
    let
        tree =
            SelectList.selected model.pages.present
                |> Zipper.toTree
    in
    H.div [ A.class "bp-3 scroll-y border-bottom flex-grow-1" ]
        [ T.restructure identity (outlineItemView model) tree
        ]


outlineItemView : Model -> Node -> List (Html Msg) -> Html Msg
outlineItemView model node children =
    let
        currentNode =
            SelectList.selected model.pages.present

        collapsed =
            isCollapsed model node

        topHint =
            H.div
                (makeDroppableIf (Common.canDropSibling node model.dragDrop)
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
                (makeDroppableIf (Common.canDropSibling node model.dragDrop)
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
                , ( "bg-primary text-white", Document.isSelected node.id currentNode )
                ]
            ]

        nodeClasses =
            [ A.classList
                [ ( "tree__label", True )
                , ( "tree__item--dropping", isDroppingInto node.id model.dragDrop )
                , ( "bg-primary text-white", Document.isSelected node.id currentNode )
                ]
            ]
    in
    case children of
        [] ->
            if Document.isPageNode node then
                emptyPageNotice model node

            else
                -- Tree leaf
                H.li
                    [ A.class "position-relative" ]
                    [ topHint
                    , if Document.isContainer node then
                        H.div
                            (nodeClasses
                                |> makeDroppableIf (Common.canDropInto node model.dragDrop) (AppendTo node.id)
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
            -- Tree node
            if Document.isPageNode node then
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
                            nodeClasses
                                |> makeDroppableIf (Common.canDropInto node model.dragDrop) (AppendTo node.id)
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


emptyPageNotice model node =
    H.div
        (A.classList
            [ ( "d-flex flex-column border border-dashed justify-content-center rounded text-center text-muted h-100", True )
            , ( "tree--dropping", isDroppingInto node.id model.dragDrop )
            ]
            :: makeDroppableIf (Common.canDropInto node model.dragDrop) (AppendTo node.id) []
        )
        []


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
    H.span [ A.class "w-100 text-truncate", clickToSelectHandler node.id ]
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


pageListView : Model -> Html Msg
pageListView model =
    H.div [ A.class "bp-3 scroll-y border-bottom", A.style "min-height" "112px", A.style "max-height" "112px" ]
        (H.div [ A.class "d-flex align-items-center justify-content-between mb-2" ]
            [ H.div [ A.class "fw-500" ]
                [ H.text "Pages" ]
            , H.button [ A.title "Add page", A.class "btn btn-link p-0 lh-1 text-dark", E.onClick <| PageAddClicked () ] [ Icons.plusCircleSmall ]
            ]
            :: SelectList.indexedMap
                (\index zipper ->
                    let
                        pageNode =
                            Zipper.root zipper
                                |> Zipper.label

                        currentNode =
                            Zipper.label zipper

                        classes =
                            A.classList
                                [ ( "page-item", True )
                                , ( "bg-primary text-white", index == 0 && currentNode == pageNode )
                                , ( "bg-gray-200", index == 0 && currentNode /= pageNode )
                                ]
                    in
                    H.div
                        [ classes
                        , E.onClick (PageSelected index)
                        , ContextMenu.open ContextMenuMsg (PageListContextPopup pageNode.id)
                        ]
                        [ H.text pageNode.name
                        ]
                )
                model.pages.present
        )


libraryView : Model -> Html Msg
libraryView _ =
    H.div [ A.class "bps-3 bpt-3 scroll-y", A.style "height" "350px", A.style "min-height" "350px" ]
        (H.div [ A.class "fw-500" ]
            [ H.text "Library" ]
            :: (Library.groups
                    |> List.map
                        (\( head, rest ) ->
                            H.section [ A.class "section mt-3" ]
                                [ H.h2 [ A.class "section__title mb-2" ]
                                    [ H.text head.group ]
                                , H.div [ A.class "d-flex flex-wrap" ]
                                    (List.map templateView (head :: rest))
                                ]
                        )
               )
        )


templateView : LibraryItem Msg -> Html Msg
templateView item =
    let
        template =
            T.label item.root
    in
    H.div
        (A.class "template bp-2 d-flex mb-1"
            :: A.title item.description
            :: DragDrop.draggable DragDropMsg (Insert item.root)
        )
        [ H.span [ A.class "me-1" ]
            [ item.icon ]
        , H.div []
            [ H.text template.name ]
        ]


pageView : Model -> Html Msg
pageView model =
    let
        tree =
            SelectList.selected model.pages.present
                |> Zipper.toTree

        ctx =
            Model.context model

        ( viewportClass, width, height ) =
            case model.viewport of
                DeviceModel name ->
                    let
                        ( w, h, _ ) =
                            Document.findDeviceInfo name
                    in
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
            H.div
                [ A.classList
                    [ ( "page", True )
                    , ( "page--design", True )
                    , ( viewportClass, True )
                    ]
                , A.attribute "data-fold" height
                , A.style "width" width
                , A.style "min-height" height
                ]
                [ content
                , H.div
                    [ A.class "page__fold"
                    , A.style "top" height
                    ]
                    [ H.text "Fold" ]
                ]

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
    E.stopPropagationOn "click" (Decode.succeed ( NodeSelected id, True ))


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
    case DragDrop.getDropId dragDrop of
        Just (AppendTo id) ->
            id == dropId

        _ ->
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
