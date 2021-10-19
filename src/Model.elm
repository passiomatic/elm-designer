module Model exposing
    ( Context
    , ContextPopup(..)
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
    , page
    , workspaceHeight
    , workspaceWidth
    )

import Bootstrap.Tab as Tab
import ContextMenu exposing (ContextMenu)
import Document exposing (..)
import File exposing (File)
import Html.Events.Extra.Wheel as Wheel
import Html5.DragDrop as DragDrop
import Http exposing (Error, Progress)
import Random
import Result exposing (Result(..))
import SelectList exposing (SelectList)
import Set exposing (Set)
import Style.Background as Background exposing (Background)
import Style.Border exposing (BorderStyle)
import Style.Font as Font exposing (..)
import Style.Input as Input exposing (LabelPosition(..))
import Style.Layout as Layout exposing (..)
import Style.Theme as Theme exposing (Theme)
import Time exposing (Posix)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UUID exposing (Seeds)
import UndoList exposing (UndoList)


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
    | TextEditingStarted String
    | CollapseNodeClicked Bool NodeId
    | PageSelected Int
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
    | WrapRowItemsChanged Bool
    | ClipboardCopyClicked
    | PageAddClicked ()
    | PageDeleteClicked NodeId
    | InsertNodeClicked (Tree Template)
    | DropDownChanged WidgetState
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
    | DragDropMsg (DragDrop.Msg DragId DropId)
    | TabMsg Tab.State
    | Undo ()
    | Redo ()
    | ContextMenuMsg (ContextMenu.Msg ContextPopup)


type ContextPopup
    = PageListContextPopup NodeId


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
    , uploadEndpoint : String

    -- , workspaceScale : Float
    -- , workspaceX : Int
    -- , workspaceY : Int
    , windowWidth : Int
    , windowHeight : Int
    , mouseX : Int
    , mouseY : Int
    , isMouseButtonDown : Bool
    , isAltDown : Bool
    , pages : UndoList (SelectList (Zipper Node))
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
    , contextMenu : ContextMenu ContextPopup
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
    { currentNode = SelectList.selected model.pages.present
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
    , uploadEndpoint : String
    , seed1 : Int
    , seed2 : Int
    , seed3 : Int
    , seed4 : Int
    }


initialModel : Flags -> ( Model, Cmd Msg )
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

        ( contextMenu, cmd ) =
            ContextMenu.init
    in
    ( { mode = DesignMode
      , uploadEndpoint = uploadEndpoint

      -- , workspaceScale = 1.0
      -- , workspaceX = -workspaceWidth // 2 + width // 2
      -- , workspaceY = 0
      , windowWidth = width
      , windowHeight = height
      , mouseX = 0
      , mouseY = 0
      , isMouseButtonDown = False
      , isAltDown = False
      , pages = UndoList.fresh <| SelectList.singleton <| Zipper.fromTree emptyDocument
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
    , Cmd.map ContextMenuMsg cmd
    )


{-| Get current selected page.
-}
page : SelectList (Zipper Node) -> Zipper Node
page pages =
    SelectList.selected pages
