module Uploader exposing (track, uploadNextFile)

{-| Upload a file using a given endpoint and tracking progress.
-}

import File exposing (File)
import Http exposing (Error)
import Model exposing (Msg(..), UploadState(..))
import Set exposing (Set)


uploadTimeout =
    60 * 1000


uploadNextFile endpoint files =
    case files of
        next :: others ->
            ( Uploading (File.name next) others 0
            , postTo endpoint next
            )

        [] ->
            ( Ready
            , Cmd.none
            )


postTo : String -> File -> Cmd Msg
postTo endpoint file =
    Http.request
        { method = "POST"
        , headers = []
        , url = endpoint
        , body =
            Http.multipartBody
                [ Http.filePart "file" file
                ]
        , expect = Http.expectString FileUploaded
        , timeout = Just uploadTimeout
        , tracker = Just (File.name file)
        }


track name others =
    Http.track name (FileUploading name others)
