module Imgbb exposing (track, uploadNextFile)

{-| Upload an image using Imgbb service and track progress.

Typical response after a successul upload is:

    {
    "data": {
        "url": "https://i.ibb.co/6NMDgSB/sample.jpg",
        "width": "2333",
        "height": "3500",
        ...
    },
    "success": true,
    "status": 200
    }

See <https://api.imgbb.com> for the full API documentation.

-}

import Document exposing (ImageData)
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


responseDecoder : Decoder ImageData
responseDecoder =
    D.map5 ImageData
        (D.at [ "data", "url" ] D.string)
        (D.succeed "")
        (D.map String.toInt (D.at [ "data", "width" ] D.string))
        (D.map String.toInt (D.at [ "data", "height" ] D.string))
        (D.at [ "data", "image", "mime" ] (D.maybe D.string))
