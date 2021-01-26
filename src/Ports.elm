port module Ports exposing
    ( copyToClipboard
    , loadDocument
    , onDocumentLoad
    , onInsertNode
    , onPageAdd
    , onPageDelete
    , onReDo
    , onUnDo
    , saveDocument
    , selectText
    , setDragImage
    , setFontLinks
    , setupAppMenu
    , showNotification
    , showPageContextMenu
    , showMessageBox
    )

import Json.Decode exposing (Value)
import Library exposing (MenuItem)
import Model exposing (..)



-- PORTS OUT


port saveDocument : String -> Cmd msg


port loadDocument : () -> Cmd msg


port copyToClipboard : String -> Cmd msg


port selectText : String -> Cmd msg


port setFontLinks : List String -> Cmd msg


port setDragImage : Value -> Cmd msg


port showPageContextMenu : String -> Cmd msg


port setupAppMenu : List MenuItem -> Cmd msg


port showNotification :
    { title : String
    , message : String
    }
    -> Cmd msg


port showMessageBox :
    { type_ : String
    , title : String
    , message : String
    , buttons : List String
    }
    -> Cmd msg



-- PORTS IN


port onDocumentLoad : (String -> msg) -> Sub msg


port onPageDelete : (String -> msg) -> Sub msg


port onPageAdd : (() -> msg) -> Sub msg


port onPageDuplicate : (String -> msg) -> Sub msg


port onInsertNode : (String -> msg) -> Sub msg


port onUnDo : (() -> msg) -> Sub msg


port onReDo : (() -> msg) -> Sub msg
