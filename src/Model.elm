module Model exposing
    ( Context
    , ContextMenuPopup(..)
    , DocumentState(..)
    , FileDrop(..)
    , Flags
    , Inspector(..)
    , Keys
    , Mode(..)
    , Model
    , Mouse
    , Msg(..)
    , UploadState(..)
    , Widget(..)
    , WidgetState(..)
    , context
    , initialModel
    , workspaceWrapperId
    )

import Bootstrap.Tab as Tab
import Browser.Dom as Dom
import ContextMenu exposing (ContextMenu)
import Document exposing (..)
import File exposing (File)
import Html.Events.Extra.Wheel as Wheel
import Html5.DragDrop as DragDrop
import Http exposing (Error, Progress)
import Random
import Result exposing (Result(..))
import Set exposing (Set)
import Style.Background as Background exposing (Background)
import Style.Border exposing (BorderStyle)
import Style.Font as Font exposing (..)
import Style.Input as Input exposing (LabelPosition(..))
import Style.Layout as Layout exposing (..)
import Style.Theme as Theme exposing (Theme)
import Task
import Time exposing (Posix)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UUID exposing (Seeds)
import UndoList exposing (UndoList)


type Msg
    = KeyChanged Bool Keys
    | MouseButtonChanged Bool Mouse
    | MouseWheelChanged Wheel.Event
    | MouseMoved Mouse
    | NodeSelected Bool NodeId
    | TextEditingStarted String
    | CollapseNodeClicked Bool NodeId
    | PaddingLockChanged Bool
    | BorderLockChanged Bool
    | WidthChanged Length
    | HeightChanged Length
    | AlignmentXChanged Alignment
    | AlignmentYChanged Alignment
    | AlignmentChanged Alignment
    | PositionChanged Position
    | TextAlignChanged TextAlignment
    | FontFamilyChanged (Local FontFamily)
    | FontWeightChanged FontWeight
    | FontSizeChanged String
    | FontColorChanged String
    | BackgroundColorChanged String
    | BackgroundChanged Background
    | BorderColorChanged String
    | BorderStyleChanged BorderStyle
    | ShadowColorChanged String
    | LabelPositionChanged LabelPosition
    | FieldEditingStarted Widget String
    | FieldEditingConfirmed
    | FieldEditingFinished
    | FieldChanged String
    | TextChanged String
    | ViewportChanged Viewport
    | PresetSizeChanged String
    | WrapRowItemsChanged Bool
    | ClipboardCopyClicked
    | InsertPageClicked
    | RemoveNodeClicked NodeId
    | InsertNodeClicked (Tree Node)
    | InsertImageClicked
    | DropDownChanged WidgetState
    | DocumentLoaded String
    | Ticked Posix
    | ModeChanged Mode
    | FileDropped NodeId File (List File)
    | FileSelected File (List File)
    | FileDragging NodeId
    | FileDragCanceled
    | FileUploading File (List File) Progress
    | FileUploaded (Result Error String)
    | DragDropMsg (DragDrop.Msg DragId DropId)
    | TabMsg Tab.State
    | Undo
    | Redo
    | ContextMenuMsg (ContextMenu.Msg ContextMenuPopup)
      --| WindowSizeChanged Int Int
    | WorkspaceSizeChanged (Result Dom.Error Dom.Viewport)
    | NoOp


type ContextMenuPopup
    = OutlinePopup NodeId


{-| All editable text fields in the app.
-}
type Widget
    = FontSizeField
    | FontColorField
    | LetterSpacingField
    | WordSpacingField
    | PaddingTopField
    | PaddingRightField
    | PaddingBottomField
    | PaddingLeftField
    | SpacingXField
    | SpacingYField
    | ImageSrcField
    | BackgroundColorField
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
    | ShadowOffsetXField
    | ShadowOffsetYField
    | ShadowBlurField
    | ShadowColorField
    | ShadowSizeField
    | InsertDropdown


type WidgetState
    = Visible Widget
    | Hidden


type Mode
    = DesignMode
    | PreviewMode


type Inspector
    = NotEdited
    | EditingField Widget String
    | EditingText


type alias Model =
    { mode : Mode
    , workspaceScale : Float
    , workspaceX : Float
    , workspaceY : Float
    , workspaceViewportWidth : Float
    , workspaceViewportHeight : Float

    -- , windowWidth : Int
    -- , windowHeight : Int
    , mouseX : Int
    , mouseY : Int
    , isMouseButtonDown : Bool
    , isAltDown : Bool
    , isMetaDown : Bool
    , document : UndoList (Zipper Node)
    , viewport : Viewport
    , inspector : Inspector
    , dragDrop : DragDrop.Model DragId DropId
    , fileDrop : FileDrop
    , rightPaneTabState : Tab.State
    , seeds : Seeds
    , currentTime : Posix
    , saveState : DocumentState
    , dropDownState : WidgetState
    , uploadState : UploadState
    , collapsedTreeItems : Set String
    , contextMenu : ContextMenu ContextMenuPopup
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
    , dragDrop : DragDrop.Model DragId DropId
    , fileDrop : FileDrop
    , inspector : Inspector
    , mode : Mode
    , theme : Theme
    }


context : Model -> Context
context model =
    { currentNode = model.document.present
    , dragDrop = model.dragDrop
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
    , seed1 : Int
    , seed2 : Int
    , seed3 : Int
    , seed4 : Int
    , platform : String
    }


isMac platform =
    platform == "MacIntel" || platform == "Macintosh"


initialModel : Flags -> ( Model, Cmd Msg )
initialModel { width, height, seed1, seed2, seed3, seed4 } =
    let
        seeds =
            Seeds
                (Random.initialSeed seed1)
                (Random.initialSeed seed2)
                (Random.initialSeed seed3)
                (Random.initialSeed seed4)

        ( newSeeds, newDocument ) =
            Document.defaultDocument seeds 1

        ( contextMenu, cmd ) =
            ContextMenu.init

        ( pageWidth, pageHeight, _ ) =
            Document.defaultDeviceInfo

        workspaceX =
            workspaceWidth / 2 - toFloat width / 2 + pageWidth / 2

        workspaceY =
            workspaceHeight / 2 - toFloat height / 2 + pageHeight / 2
    in
    ( { mode = DesignMode
      , workspaceScale = 1.0
      , workspaceX = workspaceX
      , workspaceY = workspaceY
      , workspaceViewportWidth = 0
      , workspaceViewportHeight = 0

      --   , windowWidth = width
      --   , windowHeight = height
      , mouseX = 0
      , mouseY = 0
      , isMouseButtonDown = False
      , isAltDown = False
      , isMetaDown = False
      , document = UndoList.fresh (Zipper.fromTree newDocument)
      , viewport = Fluid
      , inspector = NotEdited
      , dragDrop = DragDrop.init
      , fileDrop = Empty
      , rightPaneTabState = Tab.customInitialState "tab-design"
      , seeds = newSeeds
      , currentTime = Time.millisToPosix 0
      , saveState = Original
      , dropDownState = Hidden
      , uploadState = Ready
      , collapsedTreeItems = Set.empty
      , contextMenu = contextMenu
      }
    , Cmd.batch
        [ Cmd.map ContextMenuMsg cmd
        , Dom.getViewportOf workspaceWrapperId
            |> Task.attempt WorkspaceSizeChanged
        , Dom.setViewportOf workspaceWrapperId workspaceX workspaceY
            |> Task.attempt (\_ -> NoOp)
        ]
    )



-- IDs


workspaceWrapperId =
    "workspace-wrapper"
