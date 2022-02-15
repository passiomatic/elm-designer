module Views.ContextMenuPopup exposing (view)

import ContextMenu exposing (Config, ContextMenu, Cursor(..), Direction(..), Item, Overflow(..))
import Html as H exposing (Html)
import Model exposing (..)


view : ContextMenu ContextMenuPopup -> Html Msg
view contextMenu =
    H.div
        []
        [ ContextMenu.view
            contextMenuConfig
            ContextMenuMsg
            toItemGroups
            contextMenu
        ]


toItemGroups : ContextMenuPopup -> List (List ( Item, Msg ))
toItemGroups context =
    case context of
        -- Context menu items for outlive view
        OutlinePopup nodeId ->
            [ [ ( ContextMenu.item "Remove" |> ContextMenu.shortcut "Del", RemoveNodeClicked nodeId )
              , ( ContextMenu.item "Duplicate", DuplicateNodeClicked nodeId )
              ]
            , [ ( ContextMenu.item "Show in Workspace", NodeSelected True nodeId )
              ]
            ]


contextMenuConfig : Config
contextMenuConfig =
    { width = 200
    , direction = RightBottom
    , overflowX = Mirror
    , overflowY = Mirror
    , containerColor = "white"
    , hoverColor = "#e9ecef" -- Gray 200
    , invertText = False
    , cursor = Pointer
    , rounded = True
    , fontFamily = "inherit"
    }
