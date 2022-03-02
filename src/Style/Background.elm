module Style.Background exposing
    ( Background(..)
    , isImage
    , isNone
    , isSolid
    , setBackground
    , solid
    )

{-| Background appearance properties.
-}

import Element exposing (Color)


type Background
    = Solid Color
    | Image String
    | None


solid : Color -> Background
solid color =
    Solid color


setBackground : Background -> { a | background : Background } -> { a | background : Background }
setBackground value node =
    { node | background = value }


isSolid value =
    case value of
        Solid _ ->
            True

        _ ->
            False


isImage value =
    case value of
        Image _ ->
            True

        _ ->
            False


isNone value =
    case value of
        None ->
            True

        _ ->
            False
