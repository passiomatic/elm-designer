port module Ports exposing
    ( copyToClipboard
    , loadDocument
    , onDocumentLoad
    , onInsertNode
    , onPageAdd
    , onPageDelete
    , saveDocument
    , selectText
    , setFontLinks
    , setupAppMenu
    , showPageContextMenu
    , setDragImage
    )

import Json.Decode as Decode exposing (Decoder, Value)
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



-- PORTS IN


port onDocumentLoad : (String -> msg) -> Sub msg


port onPageDelete : (String -> msg) -> Sub msg


port onPageAdd : (() -> msg) -> Sub msg


port onPageDuplicate : (String -> msg) -> Sub msg


port onInsertNode : (String -> msg) -> Sub msg
