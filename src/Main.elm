module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BE
import CodeGen
import Codecs
import ContextMenu exposing (ContextMenu)
import Dict exposing (Dict)
import Document exposing (DragId(..), DropId(..), Node, Viewport(..))
import File exposing (File)
import File.Select as Select
import Fonts
import Html.Events as E
import Html5.DragDrop as DragDrop
import Http exposing (Progress(..))
import Json.Decode as Decode exposing (Decoder, Value)
import Library
import Maybe
import Model exposing (..)
import Ports
import SelectList exposing (SelectList)
import Set exposing (Set)
import Style.Border as Border exposing (BorderStyle(..))
import Style.Font as Font exposing (..)
import Style.Layout as Layout exposing (..)
import Style.Shadow as Shadow exposing (Shadow, ShadowType(..))
import Style.Theme as Theme exposing (Theme)
import Task
import Time
import Time.Extra as Time exposing (Interval(..))
import Tree as T exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UUID exposing (Seeds)
import UndoList
import Uploader
import Views.Common as Common
import Views.Editor as Editor


saveInterval =
    3


appName =
    "Elm Designer"


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        links =
            Fonts.links

        ( model, cmd ) =
            Model.initialModel flags
    in
    ( model
    , Cmd.batch
        [ Ports.loadDocument ()
        , Ports.setFontLinks links
        , cmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ContextMenuMsg msg_ ->
            let
                ( contextMenu, cmd ) =
                    ContextMenu.update msg_ model.contextMenu
            in
            ( { model | contextMenu = contextMenu }
            , Cmd.map ContextMenuMsg cmd
            )

        -- ###########
        -- Image drag & drop from local filesystem
        -- ###########
        FileDragging nodeId ->
            ( { model
                | fileDrop = DraggingOn nodeId
              }
            , Cmd.none
            )

        FileDragCanceled ->
            ( { model
                | fileDrop = Empty
              }
            , Cmd.none
            )

        FileSelected file files ->
            let
                node =
                    selectedPage model.pages.present
                        |> Zipper.tree
                        |> T.label

                ( newUploadState, cmd ) =
                    file
                        :: files
                        |> acceptFiles
                        |> Uploader.uploadNextFile model.uploadEndpoint
            in
            ( { model
                | uploadState = newUploadState
                , fileDrop = DroppedInto node.id -- Simulate a drag and drop operation
              }
            , cmd
            )

        FileDropped nodeId file files ->
            let
                ( newUploadState, cmd ) =
                    (file :: files)
                        |> acceptFiles
                        |> Uploader.uploadNextFile model.uploadEndpoint
            in
            ( { model
                | uploadState = newUploadState
                , fileDrop = DroppedInto nodeId
              }
            , cmd
            )

        FileUploading current others progress ->
            case progress of
                Sending sent ->
                    ( { model
                        | uploadState = Uploading current others (Http.fractionSent sent)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FileUploaded result ->
            case result of
                Ok url ->
                    let
                        ( newSeeds, newNode ) =
                            Document.imageNode (String.trim url) model.seeds

                        zipper =
                            selectedPage model.pages.present

                        newPage =
                            case model.fileDrop of
                                DroppedInto parentId ->
                                    Document.selectNodeWith parentId zipper
                                        |> Maybe.map (Document.insertNode newNode)
                                        |> Maybe.withDefault zipper

                                _ ->
                                    zipper

                        ( newUploadState, cmd ) =
                            case model.uploadState of
                                Uploading _ others _ ->
                                    Uploader.uploadNextFile model.uploadEndpoint others

                                _ ->
                                    ( Ready, Cmd.none )
                    in
                    ( { model
                        | uploadState = newUploadState
                        , fileDrop =
                            if newUploadState == Ready then
                                Empty

                            else
                                model.fileDrop
                        , pages = UndoList.mapPresent (SelectList.replaceSelected newPage) model.pages
                        , seeds = newSeeds
                        , saveState = Changed model.currentTime
                      }
                    , cmd
                    )

                Err _ ->
                    ( model
                    , showErrorBox "Could not upload image"
                    )

        -- ###########
        -- Saving
        -- ###########
        Ticked now ->
            let
                ( newSaveState, cmd ) =
                    case model.saveState of
                        Changed since ->
                            -- Save only if document hasn't been modified in saveInterval seconds
                            if Time.diff Second Time.utc since now > saveInterval then
                                let
                                    doc =
                                        { schemaVersion = Document.schemaVersion
                                        , lastUpdatedOn = now
                                        , pages =
                                            model.pages.present
                                                |> SelectList.toList
                                                |> List.map Zipper.toTree
                                        , viewport = model.viewport
                                        , collapsedTreeItems = model.collapsedTreeItems
                                        }
                                in
                                ( Saved now, serializeDocument doc )

                            else
                                ( model.saveState, Cmd.none )

                        Saved since ->
                            ( model.saveState, Cmd.none )

                        Original ->
                            ( model.saveState, Cmd.none )
            in
            ( { model
                | currentTime = now
                , saveState = newSaveState
              }
            , cmd
            )

        InsertPageClicked ->
            let
                ( newSeeds, page ) =
                    Document.emptyPageNode model.seeds (SelectList.length model.pages.present + 1)

                newPages =
                    model.pages.present
                        |> SelectList.selectLast
                        |> SelectList.insertBefore (Zipper.fromTree page)
            in
            ( { model
                | seeds = newSeeds
                , pages = UndoList.new newPages model.pages
                , saveState = Changed model.currentTime
                , dropDownState = Hidden
              }
            , Cmd.none
            )

        PageDeleteClicked id ->
            let
                isPage : Zipper Node -> Bool
                isPage zipper =
                    (Zipper.toTree zipper |> T.label).id == id

                newPages =
                    model.pages.present
                        |> SelectList.attempt (SelectList.selectBeforeIf isPage)
                        |> SelectList.attempt (SelectList.selectAfterIf isPage)
                        |> SelectList.attempt SelectList.delete
            in
            ( { model
                | pages = UndoList.new newPages model.pages
                , saveState = Changed model.currentTime
              }
            , Cmd.none
            )

        -- InsertNodeClicked label ->
        --     case Library.findTemplate label of
        --         Just template ->
        --             let
        --                 ( newSeeds, newNode ) =
        --                     Document.fromTemplate template model.seeds
        --                 newPage =
        --                     selectedPage model.pages.present
        --                         |> Document.insertNode newNode
        --                 newPages =
        --                     SelectList.replaceSelected newPage model.pages.present
        --             in
        --             ( { model
        --                 | pages = UndoList.new newPages model.pages
        --                 , saveState = Changed model.currentTime
        --                 , seeds = newSeeds
        --               }
        --             , Cmd.none
        --             )
        --         Nothing ->
        --             ( model, Cmd.none )
        InsertNodeClicked template ->
            let
                ( newSeeds, newNode ) =
                    Document.fromTemplate template model.seeds

                newPage =
                    selectedPage model.pages.present
                        |> Document.insertNode newNode

                newPages =
                    SelectList.replaceSelected newPage model.pages.present
            in
            ( { model
                | pages = UndoList.new newPages model.pages
                , saveState = Changed model.currentTime
                , seeds = newSeeds
                , dropDownState = Hidden
              }
            , Cmd.none
            )

        InsertImageClicked ->
            ( { model | dropDownState = Hidden }, Select.files acceptedTypes FileSelected )

        ClipboardCopyClicked ->
            let
                code =
                    selectedPage model.pages.present
                        |> Zipper.tree
                        |> CodeGen.emit Theme.defaultTheme model.viewport
            in
            ( model
            , Ports.copyToClipboard code
            )

        DocumentLoaded value ->
            case Codecs.fromString value of
                Ok document ->
                    case List.map Zipper.fromTree document.pages of
                        head :: rest ->
                            -- Select the first page of the list
                            ( { model
                                | pages = UndoList.mapPresent (\_ -> SelectList.fromLists [] head rest) model.pages
                                , viewport = document.viewport
                                , saveState = Original
                              }
                            , Cmd.none
                            )

                        [] ->
                            ( model, Cmd.none )

                Err reason ->
                    -- let
                    --     _ = Debug.log "Error loading document:" (Decode.errorToString reason)
                    -- in
                    ( model
                    , showErrorBox "Error loading document (perhaps schema has changed?)"
                    )

        CollapseNodeClicked collapse id ->
            let
                updater =
                    if collapse then
                        Set.insert

                    else
                        Set.remove
            in
            ( { model
                | collapsedTreeItems = updater (Document.nodeId id) model.collapsedTreeItems
              }
            , Cmd.none
            )

        TabMsg state ->
            ( { model | rightPaneTabState = state }
            , Cmd.none
            )

        DropDownChanged state ->
            ( { model | dropDownState = state }
            , Cmd.none
            )

        PageSelected index ->
            ( { model
                | pages =
                    UndoList.mapPresent
                        (\pages ->
                            case SelectList.selectBy index pages of
                                Just pages_ ->
                                    SelectList.updateSelected Zipper.root pages_

                                Nothing ->
                                    -- Fall back to first page
                                    SelectList.selectHead pages
                        )
                        model.pages

                -- Quit editing when user selects a new page
                , inspector = NotEdited
              }
            , Cmd.none
            )

        NodeSelected id ->
            ( { model
                | pages =
                    UndoList.mapPresent
                        (\pages ->
                            SelectList.updateSelected
                                (\page ->
                                    Document.selectNodeWith id page
                                        -- Fallback to root node if given node cannot be found
                                        |> Maybe.withDefault (Zipper.root page)
                                )
                                pages
                        )
                        model.pages

                -- Quit editing when user selects a new node
                , inspector = NotEdited
              }
            , Cmd.none
            )

        TextEditingStarted editorId ->
            ( { model
                | inspector = EditingText
              }
            , Cmd.batch
                [ focusElement editorId
                , Ports.selectText editorId
                ]
            )

        TextChanged value ->
            applyChange model Document.applyText value

        PaddingLockChanged value ->
            applyChange model Document.applyPaddingLock value

        BorderLockChanged value ->
            applyChange model Document.applyBorderLock value

        FieldChanged newValue ->
            case model.inspector of
                EditingField field _ ->
                    ( { model | inspector = EditingField field newValue }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FieldEditingStarted field oldValue ->
            ( { model
                | inspector = EditingField field oldValue
              }
            , Cmd.none
            )

        FieldEditingConfirmed ->
            updateField model

        FieldEditingFinished ->
            let
                ( newModel, cmd ) =
                    updateField model
            in
            ( { newModel | inspector = NotEdited }, cmd )

        WrapRowItemsChanged value ->
            applyChange model Document.applyWrapRowItems value

        TextAlignChanged value ->
            applyChange model Document.applyTextAlign value

        LabelPositionChanged value ->
            applyChange model Document.applyLabelPosition value

        FontWeightChanged value ->
            applyChange model Document.applyFontWeight value

        FontSizeChanged value ->
            let
                ( newModel, cmd ) =
                    applyChange model Document.applyFontSize value
            in
            ( { newModel | dropDownState = Hidden }, cmd )

        FontFamilyChanged family ->
            applyChange model Document.applyFontFamily family

        BorderStyleChanged value ->
            applyChange model Document.applyBorderStyle value

        BackgroundColorChanged value ->
            applyChange model Document.applyBackgroundColor value

        BackgroundChanged value ->
            applyChange model Document.applyBackground value

        BorderColorChanged value ->
            applyChange model Document.applyBorderColor value

        ShadowColorChanged value ->
            applyChange model Document.applyShadowColor value

        FontColorChanged value ->
            applyChange model Document.applyFontColor value

        AlignmentXChanged value ->
            applyChange model Document.applyAlignX value

        AlignmentYChanged value ->
            applyChange model Document.applyAlignY value

        AlignmentChanged value ->
            applyChange model Document.applyAlign value

        HeightChanged value ->
            applyChange model Document.applyHeight value

        WidthChanged value ->
            applyChange model Document.applyWidth value

        PositionChanged value ->
            applyChange model Document.applyPosition value

        DragDropMsg msg_ ->
            let
                ( newDragDrop, dragDropResult ) =
                    DragDrop.update msg_ model.dragDrop

                ( newSeeds, newPages, hasNewUndo ) =
                    case dragDropResult of
                        Just ( dragId, dropId, _ ) ->
                            let
                                ( newSeeds_, maybeNode, newZipper ) =
                                    getDroppedNode model dragId
                            in
                            case maybeNode of
                                Just node ->
                                    ( newSeeds_, addDroppedNode model dropId node newZipper, True )

                                Nothing ->
                                    ( model.seeds, selectedPage model.pages.present, False )

                        Nothing ->
                            -- Still going/failed drag and drop operation
                            ( model.seeds, selectedPage model.pages.present, False )

                newPages_ =
                    SelectList.replaceSelected newPages model.pages.present
            in
            ( { model
                | dragDrop = newDragDrop
                , pages =
                    if hasNewUndo then
                        UndoList.new newPages_ model.pages

                    else
                        UndoList.mapPresent (\_ -> newPages_) model.pages
                , seeds = newSeeds
                , saveState = Changed model.currentTime
              }
            , DragDrop.getDragstartEvent msg_
                |> Maybe.map (.event >> Ports.setDragImage)
                |> Maybe.withDefault Cmd.none
            )

        ViewportChanged viewport ->
            ( { model
                | viewport = viewport
                , saveState = Changed model.currentTime
              }
            , Cmd.none
            )

        -- Disable preview mode for now
        -- ModeChanged mode ->
        --     ( { model | mode = mode }, Cmd.none )
        KeyChanged isDown keys ->
            case ( isDown, keys.key, model.inspector ) of
                -- ############
                -- Delete node
                -- ############
                ( False, "Backspace", NotEdited ) ->
                    -- TODO remove node from model.collapsedTreeItems
                    ( { model
                        | pages = UndoList.new (SelectList.updateSelected Document.removeNode model.pages.present) model.pages
                        , saveState = Changed model.currentTime
                      }
                    , Cmd.none
                    )

                -- ############
                -- Toggle preview/design mode
                -- ############
                -- ( False, "p", NotEdited ) ->
                --     ( { model | mode = PreviewMode }, Cmd.none )
                ( False, "Escape", NotEdited ) ->
                    ( { model | mode = DesignMode }, Cmd.none )

                -- ############
                -- Stop field and inline editing
                -- ############
                ( False, "Escape", EditingField field _ ) ->
                    ( { model | inspector = NotEdited }, unfocusElement (Common.widgetId field) )

                ( False, "Escape", EditingText ) ->
                    ( { model | inspector = NotEdited }, Cmd.none )

                ( _, "Alt", NotEdited ) ->
                    ( { model | isAltDown = isDown }, Cmd.none )

                ( _, "Meta", NotEdited ) ->
                    ( { model | isMetaDown = isDown }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseButtonChanged isButtonDown mouse ->
            -- Left button only
            if mouse.button == 0 then
                ( { model | isMouseButtonDown = isButtonDown }, Cmd.none )

            else
                ( model, Cmd.none )

        Undo _ ->
            ( { model | pages = UndoList.undo model.pages }, Cmd.none )

        Redo _ ->
            ( { model | pages = UndoList.redo model.pages }, Cmd.none )

        -- MouseMoved mouse ->
        --     if model.isMouseButtonDown && model.mode == PanMode then
        --         -- Pan away
        --         let
        --             newWorkspaceX =
        --                 model.workspaceX + mouse.movementX

        --             newWorkspaceY =
        --                 model.workspaceY + mouse.movementY

        --             minLeft =
        --                 -Model.workspaceWidth // 2

        --             minTop =
        --                 -Model.workspaceHeight + model.windowHeight

        --             newModel =
        --                 { model
        --                     | workspaceX = clamp minLeft 0 newWorkspaceX
        --                     , workspaceY = clamp minTop 0 newWorkspaceY

        --                     -- , mouseX = mouse.movementX
        --                     -- , mouseY = mouse.movementY
        --                 }
        --         in
        --         ( newModel
        --         , Cmd.none
        --         )

        --     else
        --         ( model, Cmd.none )

        -- ###########
        -- Zoom away
        -- ###########

        MouseWheelChanged wheel ->
            let
                ( mouseX, mouseY ) =
                    wheel.mouseEvent.pagePos

                newModel =
                    if model.isMetaDown then
                        { model
                            | workspaceScale = clamp minWorkspaceScale maxWorkspaceScale (model.workspaceScale + wheel.deltaY * wheelSensibility)
                            , mouseX = round mouseX
                            , mouseY = round mouseY
                        }

                    else
                        model
            in
            ( newModel
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


minWorkspaceScale =
    0.1


maxWorkspaceScale =
    5.0


wheelSensibility =
    0.01


updateField model =
    case model.inspector of
        -- ###########
        -- Label
        -- ###########
        EditingField LabelField newValue ->
            applyChange model Document.applyLabel newValue

        -- ###########
        -- Width
        -- ###########
        EditingField WidthPxField newValue ->
            applyChange model
                (Document.applyWidthWith
                    (\value _ ->
                        case value of
                            Just value_ ->
                                Layout.px value_

                            Nothing ->
                                Unspecified
                    )
                )
                newValue

        EditingField WidthPortionField newValue ->
            applyChange model
                (Document.applyWidthWith
                    (\value _ ->
                        case value of
                            Just value_ ->
                                Layout.portion value_

                            Nothing ->
                                Layout.portion 1
                    )
                )
                newValue

        EditingField WidthMinField newValue ->
            applyChange model Document.applyWidthMin newValue

        EditingField WidthMaxField newValue ->
            applyChange model Document.applyWidthMax newValue

        -- ###########
        -- Height
        -- ###########
        EditingField HeightPxField newValue ->
            applyChange model
                (Document.applyHeightWith
                    (\value _ ->
                        case value of
                            Just value_ ->
                                Layout.px value_

                            Nothing ->
                                Unspecified
                    )
                )
                newValue

        EditingField HeightPortionField newValue ->
            applyChange model
                (Document.applyHeightWith
                    (\value _ ->
                        case value of
                            Just value_ ->
                                Layout.portion value_

                            Nothing ->
                                Layout.portion 1
                    )
                )
                newValue

        EditingField HeightMinField newValue ->
            applyChange model Document.applyHeightMin newValue

        EditingField HeightMaxField newValue ->
            applyChange model Document.applyHeightMax newValue

        -- ###########
        -- Transformation
        -- ###########
        EditingField OffsetXField newValue ->
            applyChange model (Document.applyOffset Layout.setOffsetX) newValue

        EditingField OffsetYField newValue ->
            applyChange model (Document.applyOffset Layout.setOffsetY) newValue

        -- ###########
        -- Font size
        -- ###########
        EditingField FontSizeField newValue ->
            applyChange model Document.applyFontSize newValue

        -- ###########
        -- Font color
        -- ###########
        EditingField FontColorField newValue ->
            applyChange model Document.applyFontColor newValue

        -- ###########
        -- Letter Spacing
        -- ###########
        EditingField LetterSpacingField newValue ->
            applyChange model Document.applyLetterSpacing newValue

        -- ###########
        -- Word Spacing
        -- ###########
        EditingField WordSpacingField newValue ->
            applyChange model Document.applyWordSpacing newValue

        -- ###########
        -- Background
        -- ###########
        EditingField BackgroundColorField newValue ->
            applyChange model Document.applyBackgroundColor newValue

        EditingField BackgroundImageField newValue ->
            applyChange model Document.applyBackgroundImage newValue

        -- ###########
        -- Borders
        -- ###########
        EditingField BorderColorField newValue ->
            applyChange model Document.applyBorderColor newValue

        EditingField BorderTopLeftCornerField newValue ->
            applyChange model (Document.applyBorderCorner Border.setTopLeftCorner) newValue

        EditingField BorderTopRightCornerField newValue ->
            applyChange model (Document.applyBorderCorner Border.setTopRightCorner) newValue

        EditingField BorderBottomRightCornerField newValue ->
            applyChange model (Document.applyBorderCorner Border.setBottomRightCorner) newValue

        EditingField BorderBottomLeftCornerField newValue ->
            applyChange model (Document.applyBorderCorner Border.setBottomLeftCorner) newValue

        EditingField BorderTopWidthField newValue ->
            applyChange model (Document.applyBorderWidth Border.setTopWidth) newValue

        EditingField BorderRightWidthField newValue ->
            applyChange model (Document.applyBorderWidth Border.setRightWidth) newValue

        EditingField BorderBottomWidthField newValue ->
            applyChange model (Document.applyBorderWidth Border.setBottomWidth) newValue

        EditingField BorderLeftWidthField newValue ->
            applyChange model (Document.applyBorderWidth Border.setLeftWidth) newValue

        -- ###########
        -- Shadow
        -- ###########
        EditingField ShadowOffsetXField newValue ->
            applyChange model (Document.applyShadow Shadow.setOffsetX) newValue

        EditingField ShadowOffsetYField newValue ->
            applyChange model (Document.applyShadow Shadow.setOffsetY) newValue

        EditingField ShadowSizeField newValue ->
            applyChange model (Document.applyShadow Shadow.setSize) newValue

        EditingField ShadowBlurField newValue ->
            applyChange model (Document.applyShadow Shadow.setBlur) newValue

        EditingField ShadowColorField newValue ->
            applyChange model Document.applyShadowColor newValue

        -- ###########
        -- Padding
        -- ###########
        EditingField PaddingTopField newValue ->
            applyChange model (Document.applyPadding Layout.setPaddingTop) newValue

        EditingField PaddingRightField newValue ->
            applyChange model (Document.applyPadding Layout.setPaddingRight) newValue

        EditingField PaddingBottomField newValue ->
            applyChange model (Document.applyPadding Layout.setPaddingBottom) newValue

        EditingField PaddingLeftField newValue ->
            applyChange model (Document.applyPadding Layout.setPaddingLeft) newValue

        -- ###########
        -- Spacing
        -- ###########
        EditingField SpacingXField newValue ->
            applyChange model (Document.applySpacing Layout.setSpacingX) newValue

        EditingField SpacingYField newValue ->
            applyChange model (Document.applySpacing Layout.setSpacingY) newValue

        _ ->
            ( model, Cmd.none )


{-| Figure out _what_ user just dropped: template or node?
-}
getDroppedNode : Model -> DragId -> ( Seeds, Maybe (Tree Node), Zipper Node )
getDroppedNode model dragId =
    let
        currentZipper =
            selectedPage model.pages.present
    in
    case dragId of
        Move node ->
            case Document.selectNodeWith node.id currentZipper of
                Just zipper ->
                    if model.isAltDown then
                        -- Duplicate node
                        let
                            ( newSeeds, newNode ) =
                                Document.duplicateNode zipper model.seeds
                        in
                        ( newSeeds, Just newNode, zipper )

                    else
                        -- Move node
                        let
                            newZipper =
                                Document.removeNode zipper
                        in
                        ( model.seeds, Just (Zipper.tree zipper), newZipper )

                Nothing ->
                    ( model.seeds, Nothing, currentZipper )

        Insert template ->
            let
                ( newSeeds, newNode ) =
                    Document.fromTemplate template model.seeds
            in
            ( newSeeds, Just newNode, currentZipper )


{-| Figure out _where_ user just dropped the node.
-}
addDroppedNode model dropId node zipper =
    case dropId of
        -- Insert new element just before the sibling
        InsertBefore siblingId ->
            Document.insertNodeBefore siblingId node zipper

        -- Insert new element just after the sibling
        InsertAfter siblingId ->
            Document.insertNodeAfter siblingId node zipper

        -- Add new element as last child
        AppendTo parentId ->
            case Document.selectNodeWith parentId zipper of
                Just zipper_ ->
                    Document.appendNode node zipper_

                Nothing ->
                    zipper


selectedPage : SelectList (Zipper Node) -> Zipper Node
selectedPage pages =
    SelectList.selected pages


applyChange : Model -> (a -> Zipper Node -> Zipper Node) -> a -> ( Model, Cmd Msg )
applyChange ({ pages, currentTime } as model) updater newValue =
    let
        newPages =
            SelectList.updateSelected (updater newValue) model.pages.present
    in
    ( { model
        | pages = UndoList.new newPages model.pages
        , saveState = Changed model.currentTime
      }
    , Cmd.none
    )


unfocusElement : String -> Cmd Msg
unfocusElement id =
    Task.attempt (\_ -> NoOp) (Dom.blur id)


focusElement : String -> Cmd Msg
focusElement id =
    Task.attempt (\_ -> NoOp) (Dom.focus id)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = Editor.view
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        uploadSub =
            case model.uploadState of
                Uploading current others _ ->
                    Uploader.track current others

                _ ->
                    Sub.none
    in
    Sub.batch
        [ BE.onKeyDown (Decode.map (KeyChanged True) keysDecoder)
        , BE.onKeyUp (Decode.map (KeyChanged False) keysDecoder)
        , BE.onMouseDown (Decode.map (MouseButtonChanged True) mouseDecoder)
        , BE.onMouseUp (Decode.map (MouseButtonChanged False) mouseDecoder)
        , Ports.onDocumentLoad DocumentLoaded
        , Time.every 1000 Ticked
        , uploadSub
        , Sub.map ContextMenuMsg (ContextMenu.subscriptions model.contextMenu)
        ]


trackMouseMove shouldTrack subs =
    if shouldTrack then
        BE.onMouseMove (Decode.map MouseMoved mouseDecoder) :: subs

    else
        subs


keysDecoder : Decoder Keys
keysDecoder =
    Decode.map6 Keys
        (Decode.field "keyCode" Decode.int)
        (Decode.field "key" Decode.string)
        (Decode.field "altKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "metaKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)


mouseDecoder : Decoder Mouse
mouseDecoder =
    Decode.map5 Mouse
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)
        (Decode.field "movementX" Decode.int)
        (Decode.field "movementY" Decode.int)
        (Decode.field "button" Decode.int)


serializeDocument document =
    document
        |> Codecs.toString
        |> Ports.saveDocument


acceptedTypes : List String
acceptedTypes =
    [ "image/jpeg", "image/png", "image/gif", "image/svg+xml" ]


acceptFiles files =
    let
        acceptedTypes_ =
            Set.fromList acceptedTypes
    in
    List.filter
        (\f ->
            Set.member (File.mime f) acceptedTypes_
        )
        files



-- NOTIFICATION


showErrorBox message =
    Ports.showMessageBox
        { type_ = "error"
        , title = appName
        , message = message
        , buttons = [ "OK" ]
        }


showMessageBox message =
    Ports.showMessageBox
        { type_ = "info"
        , title = appName
        , message = message
        , buttons = [ "OK" ]
        }
