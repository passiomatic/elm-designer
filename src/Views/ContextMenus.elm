module Views.ContextMenus exposing (pageListView)

import ContextMenu exposing (Config, Cursor(..), Direction(..), Item, Overflow(..))
import Html as H exposing (Attribute, Html)
import Model exposing (..)


pageListView : Model -> Html Msg
pageListView model =
    H.div
        []
        [ ContextMenu.view
            contextMenuConfig
            ContextMenuMsg
            toItemGroups
            model.contextMenu
        ]


toItemGroups : ContextPopup -> List (List ( Item, Msg ))
toItemGroups context =
    case context of
        PageListContextPopup nodeId ->
            [ [ ( ContextMenu.item "Delete page", PageDeleteClicked nodeId )
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
