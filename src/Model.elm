module Model exposing
    ( Context
    , DocumentState(..)
    , Field(..)
    , FileDrop(..)
    , Flags
    , Inspector(..)
    , Keys
    , Mode(..)
    , Model
    , Mouse
    , Msg(..)
    , UploadState(..)
    , WidgetState(..)
    , context
    , initialModel
    , page
    , workspaceHeight
    , workspaceWidth
    )

import Bootstrap.Tab as Tab
import Demo
import Dict exposing (Dict)
import Document exposing (..)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node
import File exposing (File)
import Html.Events.Extra.Wheel as Wheel
import Html5.DragDrop as DragDrop
import Http exposing (Error, Progress)
import Loader
import Random
import Result exposing (Result(..))
import SelectList exposing (SelectList)
import Set exposing (Set)
import Style.Background as Background exposing (Background)
import Style.Font as Font exposing (..)
import Style.Layout as Layout exposing (..)
import Style.Theme as Theme exposing (Theme)
import Time exposing (Posix)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UUID exposing (Seeds)


workspaceWidth =
    4000


workspaceHeight =
    4000


type Msg
    = KeyChanged Bool Keys
    | MouseButtonChanged Bool Mouse
    | MouseWheelChanged Wheel.Event
    | MouseMoved Mouse
    | NodeSelected NodeId
    | TextEditingStarted NodeId
    | CollapseNodeClicked Bool NodeId
    | PageSelected Int
    | PaddingLockChanged Bool
    | BorderLockChanged Bool
    | WidthChanged Length
    | HeightChanged Length
    | AlignmentXChanged Alignment
    | AlignmentYChanged Alignment
    | AlignmentChanged Alignment
    | TextAlignChanged TextAlignment
    | FontFamilyChanged (Local FontFamily)
    | FontWeightChanged FontWeight
    | FontSizeChanged String
    | FontColorChanged String
    | BackgroundColorChanged String
    | BackgroundSizingChanged Background
    | BorderColorChanged String
    | FieldEditingStarted Field String
    | FieldEditingFinished
    | FieldChanged String
    | TextChanged String
    | ViewportChanged Viewport
    | WrapRowItemsChanged Bool
    | ClipboardCopyClicked
    | PageAddClicked ()
    | PageContextMenuClicked NodeId
    | PageDeleteClicked String
    | InsertNodeClicked String
    | DropDownChanged WidgetState
      --| LoadDocument
    | DocumentLoaded String
    | Ticked Posix
    | ModeChanged Mode
    | FileDropped NodeId File (List File)
    | FileSelected (List File)
    | FileDragging NodeId
    | FileDragCanceled
    | FileUploading File (List File) Progress
    | FileUploaded (Result Error String)
    | NoOp
    | BindingDragDropMsg (DragDrop.Msg BindId NodeId)
    | DragDropMsg (DragDrop.Msg DragId DropId)
    | TabMsg Tab.State


{-| All editable text fields in the app.
-}
type Field
    = FontSizeField
    | FontColorField
    | LetterSpacingField
    | WordSpacingField
    | BackgroundColorField
    | PaddingTopField
    | PaddingRightField
    | PaddingBottomField
    | PaddingLeftField
    | SpacingXField
    | SpacingYField
    | ImageSrcField
    | BackgroundImageField
    | BorderColorField
    | BorderTopWidthField
    | BorderRightWidthField
    | BorderBottomWidthField
    | BorderLeftWidthField
    | BorderTopLeftCornerField
    | BorderTopRightCornerField
    | BorderBottomRightCornerField
    | BorderBottomLeftCornerField
    | LabelField
    | OffsetXField
    | OffsetYField
    | WidthMinField
    | WidthMaxField
    | WidthPxField
    | WidthPortionField
    | HeightMinField
    | HeightMaxField
    | HeightPxField
    | HeightPortionField


type WidgetState
    = Visible Field
    | Hidden


type Mode
    = DesignMode
    | PreviewMode


type Inspector
    = NotEdited
    | EditingField Field String String
    | EditingText


type alias Model =
    { mode : Mode
    , uploadEndpoint : String
    , types : Dict String (Node.Node Declaration)

    -- , workspaceScale : Float
    -- , workspaceX : Int
    -- , workspaceY : Int
    , windowWidth : Int
    , windowHeight : Int
    , mouseX : Int
    , mouseY : Int
    , isMouseButtonDown : Bool
    , isAltDown : Bool
    , pages : SelectList (Zipper Node)
    , viewport : Viewport
    , inspector : Inspector
    , dragDrop : DragDrop.Model DragId DropId
    , bindingDragDrop : DragDrop.Model BindId NodeId
    , fileDrop : FileDrop
    , rightPaneTabState : Tab.State
    , seeds : Seeds
    , currentTime : Posix
    , saveState : DocumentState
    , dropDownState : WidgetState
    , uploadState : UploadState
    , collapsedTreeItems : Set String
    }


type FileDrop
    = DraggingOn NodeId
    | DroppedInto NodeId
    | Empty


type UploadState
    = Uploading File (List File) Float
    | Ready


type DocumentState
    = Saved Posix
    | Changed Posix
    | Original


{-| Rendering context used for Elm UI nodes.
-}
type alias Context =
    { currentNode : Zipper Node
    , nodeDragDrop : DragDrop.Model DragId DropId
    , bindingDragDrop : DragDrop.Model BindId NodeId
    , fileDrop : FileDrop
    , inspector : Inspector
    , mode : Mode
    , theme : Theme
    }


context : Model -> Context
context model =
    { currentNode = SelectList.selected model.pages
    , nodeDragDrop = model.dragDrop
    , bindingDragDrop = model.bindingDragDrop
    , fileDrop = model.fileDrop
    , inspector = model.inspector
    , mode = model.mode
    , theme = Theme.defaultTheme
    }


type alias Keys =
    { keyCode : Int
    , key : String
    , altOn : Bool
    , ctrlOn : Bool
    , metaOn : Bool
    , shiftOn : Bool
    }


type alias Mouse =
    { clientX : Int
    , clientY : Int
    , movementX : Int
    , movementY : Int
    , button : Int
    }



-- INIT


type alias Flags =
    { width : Int
    , height : Int
    , uploadEndpoint : String
    , seed1 : Int
    , seed2 : Int
    , seed3 : Int
    , seed4 : Int
    }


initialModel : Flags -> Model
initialModel { width, height, uploadEndpoint, seed1, seed2, seed3, seed4 } =
    let
        seeds =
            Seeds
                (Random.initialSeed seed1)
                (Random.initialSeed seed2)
                (Random.initialSeed seed3)
                (Random.initialSeed seed4)

        ( newSeeds, emptyDocument ) =
            Document.emptyPageNode seeds 1

        declarations =
            loadDemo
                |> Dict.fromList
    in
    { mode = DesignMode
    , uploadEndpoint = uploadEndpoint
    , types = declarations

    -- , workspaceScale = 1.0
    -- , workspaceX = -workspaceWidth // 2 + width // 2
    -- , workspaceY = 0
    , windowWidth = width
    , windowHeight = height
    , mouseX = 0
    , mouseY = 0
    , isMouseButtonDown = False
    , isAltDown = False
    , pages = SelectList.singleton (Zipper.fromTree emptyDocument)
    , viewport = Fluid
    , inspector = NotEdited
    , dragDrop = DragDrop.init
    , bindingDragDrop = DragDrop.init
    , fileDrop = Empty
    , rightPaneTabState = Tab.customInitialState "tab-design"
    , seeds = newSeeds
    , currentTime = Time.millisToPosix 0
    , saveState = Original
    , dropDownState = Hidden
    , uploadState = Ready
    , collapsedTreeItems = Set.empty
    }


{-| Get current selected page.
-}
page : SelectList (Zipper Node) -> Zipper Node
page pages =
    SelectList.selected pages


loadDemo : List ( String, Node.Node Declaration )
loadDemo =
    let
        result =
            Loader.parse Demo.source [ Demo.source2 ]
    in
    case result of
        Ok file ->
            Loader.declarations file

        Err deadEnds ->
            let
                _ =
                    Debug.log "Cannot parse Elm file." deadEnds
            in
            []
