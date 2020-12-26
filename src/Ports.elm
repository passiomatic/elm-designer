port module Ports exposing
    ( copyToClipboard
    , loadDocument
    , onDocumentLoad
    , onPageDelete
    , saveDocument
    , selectText
    , setFontLinks
    , showPageContextMenu
    , startDrag
    )

import Json.Decode as Decode exposing (Decoder, Value)
import Model exposing (..)



-- PORTS OUT


port saveDocument : String -> Cmd msg


port loadDocument : () -> Cmd msg


port copyToClipboard : String -> Cmd msg


port selectText : String -> Cmd msg


port setFontLinks : List String -> Cmd msg


port startDrag : Value -> Cmd msg


port showPageContextMenu : String -> Cmd msg



-- PORTS IN


port onDocumentLoad : (String -> msg) -> Sub msg


port onPageDelete : (String -> msg) -> Sub msg
