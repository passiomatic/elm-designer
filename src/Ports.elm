port module Ports exposing
    ( copyToClipboard
    , endDrag
    , loadDocument
    , onDocumentChange
    , onDocumentLoad
    , openPreview
    , saveDocument
    , selectText
    , setDragImage
    , setFontLinks
    , showNotification
    )

import Json.Decode exposing (Value)
import Model exposing (..)



-- PORTS OUT


port openPreview : () -> Cmd msg


port saveDocument : String -> Cmd msg


port loadDocument : () -> Cmd msg


port copyToClipboard : String -> Cmd msg


port selectText : String -> Cmd msg


port setFontLinks : List String -> Cmd msg


port setDragImage : { event : Value, dragging : Bool } -> Cmd msg


port endDrag : () -> Cmd msg


port showNotification :
    { title : String
    , message : String
    }
    -> Cmd msg



-- PORTS IN


port onDocumentLoad : (String -> msg) -> Sub msg


port onDocumentChange : (String -> msg) -> Sub msg
