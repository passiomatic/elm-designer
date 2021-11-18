module Imgbb exposing (track, uploadNextFile)

{-| Upload an image using Imgbb service and track progress.

    See: https://api.imgbb.com

-}

import File exposing (File)
import Http exposing (Error)
import Json.Decode as D exposing (Decoder)
import Model exposing (Msg(..), UploadState(..))


endpointUrl =
    -- Set expiration to 180 days
    "https://api.imgbb.com/1/upload?expiration=15552000&key="


uploadTimeout =
    60 * 1000


uploadNextFile key files =
    case files of
        next :: others ->
            ( Uploading next others 0
            , postTo key next
            )

        [] ->
            ( Ready
            , Cmd.none
            )


postTo : String -> File -> Cmd Msg
postTo key file =
    Http.request
        { method = "POST"
        , headers = []
        , url = endpointUrl ++ key
        , body =
            Http.multipartBody
                [ Http.filePart "image" file
                ]
        , expect = Http.expectJson FileUploaded responseDecoder
        , timeout = Just uploadTimeout
        , tracker = Just (File.name file)
        }


track current others =
    Http.track (File.name current) (FileUploading current others)


responseDecoder : Decoder String
responseDecoder =
    D.at [ "data", "url" ] D.string
