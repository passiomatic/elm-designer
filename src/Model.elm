module Model exposing
    ( Context
    , DocumentState(..)
    , Field(..)
    , Flags
    , Inspector(..)
    , Keys
    , Mode(..)
    , Model
    , Mouse
    , Msg(..)
    , WidgetState(..)
    , context
    , initialModel
    , page
    , workspaceHeight
    , workspaceWidth
    )

import Bootstrap.Tab as Tab
import Codecs
import Document exposing (..)
import Html as H exposing (Html)
import Html.Events.Extra.Wheel as Wheel
import Html5.DragDrop as DragDrop
import Icons
import Random
import Result exposing (Result(..))
import SelectList exposing (SelectList)
import Set exposing (Set)
import Style.Layout as Layout exposing (..)
import Style.Background as Background exposing (Background)
import Style.Font as Font exposing (..)
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
    | NoOp
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
    , rightPaneTabState : Tab.State
    , seeds : Seeds
    , currentTime : Posix
    , saveState : DocumentState
    , alerts : List String
    , dropDownState : WidgetState
    , collapsedTreeItems : Set String
    }


type DocumentState
    = Saved Posix
    | Changed Posix
    | Original


{-| Rendering context used for Elm UI nodes.
-}
type alias Context =
    { currentNode : Zipper Node
    , dragDrop : DragDrop.Model DragId DropId
    , inspector : Inspector
    , mode : Mode
    , theme : Theme
    }


context : Model -> Context
context model =
    { currentNode = SelectList.selected model.pages
    , dragDrop = model.dragDrop
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
    }


initialModel : Flags -> Model
initialModel { width, height, seed1, seed2, seed3, seed4 } =
    let
        seeds =
            Seeds
                (Random.initialSeed seed1)
                (Random.initialSeed seed2)
                (Random.initialSeed seed3)
                (Random.initialSeed seed4)

        ( newSeeds, emptyDocument ) =
            Document.emptyPageNode seeds 1
    in
    { mode = DesignMode

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
    , rightPaneTabState = Tab.customInitialState "tab-design"
    , seeds = newSeeds
    , currentTime = Time.millisToPosix 0
    , saveState = Original
    , alerts = []
    , dropDownState = Hidden
    , collapsedTreeItems = Set.empty
    }


{-| Get current selected page.
-}
page : SelectList (Zipper Node) -> Zipper Node
page pages =
    SelectList.selected pages
