module Main exposing (main)

import Array
import Bootstrap.Tab as Tab
import Browser
import Browser.Dom as Dom
import Browser.Events as BE
import CodeGen
import Codecs
import Dict exposing (Dict)
import Document exposing (DragId(..), DropId(..), Node, Viewport(..))
import File exposing (File)
import Fonts
import Html.Events as E
import Html.Events.Extra.Mouse
import Html5.DragDrop as DragDrop
import Http exposing (Progress(..))
import Icons
import Json.Decode as Decode exposing (Decoder, Value)
import Library
import Maybe
import Model exposing (..)
import Ports
import Random exposing (Seed)
import SelectList exposing (SelectList)
import Set exposing (Set)
import Style.Border as Border exposing (BorderCorner, BorderStyle(..), BorderWidth)
import Style.Font as Font exposing (..)
import Style.Layout as Layout exposing (..)
import Style.Theme as Theme exposing (Theme)
import Task
import Time
import Time.Extra as Time exposing (Interval(..))
import Tree as T exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UUID exposing (Seeds)
import UndoList
import Uploader
import Views.Common as Common exposing (fieldId)
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
    in
    ( Model.initialModel flags
    , Cmd.batch
        [ Ports.loadDocument ()
        , Ports.setFontLinks links
        , Ports.setupAppMenu Library.menuItems
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        FileSelected files ->
            let
                node =
                    selectedPage model.pages.present
                        |> Zipper.tree
                        |> T.label

                ( newUploadState, cmd ) =
                    files
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

        PageContextMenuClicked id ->
            ( model
            , Ports.showPageContextMenu (Document.nodeId id)
            )

        PageAddClicked _ ->
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
              }
            , Cmd.none
            )

        PageDeleteClicked id ->
            let
                isPage : Zipper Node -> Bool
                isPage zipper =
                    Document.nodeId (Zipper.toTree zipper |> T.label).id == id

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

        InsertNodeClicked label ->
            case Library.findTemplate label of
                Just template ->
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
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

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

        TextEditingStarted id ->
            let
                elementId =
                    Document.nodeId id
            in
            ( { model
                | inspector = EditingText
              }
            , Cmd.batch
                [ focusElement elementId
                , Ports.selectText elementId
                ]
            )

        TextChanged value ->
            applyChange model Document.applyText value

        PaddingLockChanged value ->
            applyChangeAndFinish model Document.applyPaddingLock value

        BorderLockChanged value ->
            applyChangeAndFinish model Document.applyBorderLock value

        FieldEditingStarted field oldValue ->
            ( { model
                | inspector = EditingField field oldValue oldValue
              }
            , Cmd.none
            )

        FieldChanged newValue ->
            case model.inspector of
                EditingField field oldValue _ ->
                    ( { model | inspector = EditingField field oldValue newValue }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FieldEditingFinished ->
            case model.inspector of
                -- ###########
                -- Label
                -- ###########
                EditingField LabelField oldValue newValue ->
                    applyChangeAndFinish model Document.applyLabel newValue

                -- ###########
                -- Width
                -- ###########
                EditingField WidthPxField oldValue newValue ->
                    applyChangeAndFinish model
                        (Document.applyWidthWith
                            (\value length ->
                                case value of
                                    Just value_ ->
                                        Layout.setStrategy (Layout.px value_) length

                                    Nothing ->
                                        Layout.setStrategy Unspecified length
                            )
                        )
                        newValue

                EditingField WidthPortionField oldValue newValue ->
                    applyChangeAndFinish model
                        (Document.applyWidthWith
                            (\value length ->
                                case value of
                                    Just value_ ->
                                        Layout.setStrategy (Layout.portion value_) length

                                    Nothing ->
                                        Layout.setStrategy (Layout.portion 1) length
                            )
                        )
                        newValue

                EditingField WidthMinField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyWidthWith Layout.setMinLength) newValue

                EditingField WidthMaxField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyWidthWith Layout.setMaxLength) newValue

                -- ###########
                -- Height
                -- ###########
                EditingField HeightPxField oldValue newValue ->
                    applyChangeAndFinish model
                        (Document.applyHeightWith
                            (\value length ->
                                case value of
                                    Just value_ ->
                                        Layout.setStrategy (Layout.px value_) length

                                    Nothing ->
                                        Layout.setStrategy Unspecified length
                            )
                        )
                        newValue

                EditingField HeightPortionField oldValue newValue ->
                    applyChangeAndFinish model
                        (Document.applyHeightWith
                            (\value length ->
                                case value of
                                    Just value_ ->
                                        Layout.setStrategy (Layout.portion value_) length

                                    Nothing ->
                                        Layout.setStrategy (Layout.portion 1) length
                            )
                        )
                        newValue

                EditingField HeightMinField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyHeightWith Layout.setMinLength) newValue

                EditingField HeightMaxField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyHeightWith Layout.setMaxLength) newValue

                -- ###########
                -- Transformation
                -- ###########
                EditingField OffsetXField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyOffset Layout.setOffsetX) newValue

                EditingField OffsetYField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyOffset Layout.setOffsetY) newValue

                -- ###########
                -- Font size
                -- ###########
                EditingField FontSizeField oldValue newValue ->
                    applyChangeAndFinish model Document.applyFontSize newValue

                -- ###########
                -- Font color
                -- ###########
                EditingField FontColorField oldValue newValue ->
                    applyChangeAndFinish model Document.applyFontColor newValue

                -- ###########
                -- Letter Spacing
                -- ###########
                EditingField LetterSpacingField oldValue newValue ->
                    applyChangeAndFinish model Document.applyLetterSpacing newValue

                -- ###########
                -- Word Spacing
                -- ###########
                EditingField WordSpacingField oldValue newValue ->
                    applyChangeAndFinish model Document.applyWordSpacing newValue

                -- ###########
                -- Background
                -- ###########
                EditingField BackgroundColorField oldValue newValue ->
                    applyChangeAndFinish model Document.applyBackgroundColor newValue

                EditingField BackgroundImageField oldValue newValue ->
                    applyChangeAndFinish model Document.applyBackgroundUrl newValue

                -- ###########
                -- Borders
                -- ###########
                EditingField BorderColorField oldValue newValue ->
                    applyChangeAndFinish model Document.applyBorderColor newValue

                EditingField BorderTopLeftCornerField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyBorderCorner Border.setTopLeftCorner) newValue

                EditingField BorderTopRightCornerField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyBorderCorner Border.setTopRightCorner) newValue

                EditingField BorderBottomRightCornerField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyBorderCorner Border.setBottomRightCorner) newValue

                EditingField BorderBottomLeftCornerField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyBorderCorner Border.setBottomLeftCorner) newValue

                EditingField BorderTopWidthField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyBorderWidth Border.setTopWidth) newValue

                EditingField BorderRightWidthField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyBorderWidth Border.setRightWidth) newValue

                EditingField BorderBottomWidthField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyBorderWidth Border.setBottomWidth) newValue

                EditingField BorderLeftWidthField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyBorderWidth Border.setLeftWidth) newValue

                -- ###########
                -- Padding
                -- ###########
                EditingField PaddingTopField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyPadding Layout.setPaddingTop) newValue

                EditingField PaddingRightField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyPadding Layout.setPaddingRight) newValue

                EditingField PaddingBottomField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyPadding Layout.setPaddingBottom) newValue

                EditingField PaddingLeftField oldValue newValue ->
                    applyChangeAndFinish model (Document.applyPadding Layout.setPaddingLeft) newValue

                -- ###########
                -- Spacing
                -- ###########
                EditingField SpacingXField oldValue newValue ->
                    applyChangeAndFinish model (Document.applySpacing Layout.setSpacingX) newValue

                EditingField SpacingYField oldValue newValue ->
                    applyChangeAndFinish model (Document.applySpacing Layout.setSpacingY) newValue

                _ ->
                    ( model, Cmd.none )

        WrapRowItemsChanged value ->
            applyChangeAndFinish model Document.applyWrapRowItems value

        TextAlignChanged value ->
            applyChangeAndFinish model Document.applyTextAlign value

        FontWeightChanged value ->
            applyChangeAndFinish model Document.applyFontWeight value

        FontSizeChanged value ->
            applyChangeAndFinish model Document.applyFontSize value

        FontFamilyChanged family ->
            applyChangeAndFinish model Document.applyFontFamily family

        BackgroundColorChanged value ->
            applyChangeAndFinish model Document.applyBackgroundColor value

        BackgroundSizingChanged value ->
            applyChangeAndFinish model Document.applyBackground value

        BorderColorChanged value ->
            applyChangeAndFinish model Document.applyBorderColor value

        FontColorChanged value ->
            applyChangeAndFinish model Document.applyFontColor value

        AlignmentXChanged value ->
            applyChangeAndFinish model Document.applyAlignX value

        AlignmentYChanged value ->
            applyChangeAndFinish model Document.applyAlignY value

        AlignmentChanged value ->
            applyChangeAndFinish model Document.applyAlign value

        HeightChanged value ->
            applyChangeAndFinish model Document.applyHeight value

        WidthChanged value ->
            applyChangeAndFinish model Document.applyWidth value

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
                ( False, "Escape", EditingField field _ _ ) ->
                    ( { model | inspector = NotEdited }, unfocusElement (fieldId field) )

                ( False, "Escape", EditingText ) ->
                    ( { model | inspector = NotEdited }, Cmd.none )

                -- Track Alt status
                ( _, "Alt", NotEdited ) ->
                    ( { model | isAltDown = isDown }, Cmd.none )

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
        -- MouseWheelChanged wheel ->
        --     -- Zoom away
        --     let
        --         ( mouseX, mouseY ) =
        --             wheel.mouseEvent.pagePos
        --         newModel =
        --             if model.isAltDown then
        --                 { model
        --                     | workspaceScale = clamp minWorkspaceScale maxWorkspaceScale (model.workspaceScale + wheel.deltaY * wheelSensibility)
        --                     , mouseX = round mouseX
        --                     , mouseY = round mouseY
        --                 }
        --             else
        --                 model
        --     in
        --     ( newModel
        --     , Cmd.none
        --     )
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


applyChangeAndFinish : Model -> (a -> Zipper Node -> Zipper Node) -> a -> ( Model, Cmd Msg )
applyChangeAndFinish ({ pages, currentTime } as model) updater newValue =
    let
        newPages =
            SelectList.updateSelected (updater newValue) model.pages.present
    in
    ( { model
        | pages = UndoList.new newPages model.pages
        , dropDownState = Hidden
        , inspector = NotEdited
        , saveState = Changed currentTime
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
        , Ports.onPageAdd PageAddClicked
        , Ports.onPageDelete PageDeleteClicked
        , Ports.onInsertNode InsertNodeClicked
        , Ports.onUndo Undo
        , Ports.onRedo Redo
        , Time.every 1000 Ticked
        , uploadSub
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


acceptedTypes : Set String
acceptedTypes =
    Set.fromList [ "image/jpeg", "image/png", "image/gif", "image/svg+xml" ]


acceptFiles files =
    List.filter
        (\f ->
            Set.member (File.mime f) acceptedTypes
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
