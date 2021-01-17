module Uploader exposing (start, track)

{-| See <https://0x0.st>
-}

import File exposing (File)
import Http exposing (Error)
import Model exposing (Msg)


start : String -> (Result Error String -> Msg) -> File -> Cmd Msg
start url msg file =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body =
            Http.multipartBody
                [ Http.filePart "file" file
                ]
        , expect = Http.expectString msg
        , timeout = Just uploadTimeout
        -- TODO Replace with actual file name?
        , tracker = Just "file-upload"
        }


track msg =
    Http.track "file-upload" msg


uploadTimeout =
    60 * 1000
