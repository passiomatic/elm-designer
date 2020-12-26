module Style.Background exposing
    ( Background(..)
    , setBackgroundColor
    , setBackground
    )

{-| Background appearance properties.
-}

import Element exposing (Color)


type Background
    = Cropped String
    | Uncropped String
    | Tiled String
    | None


setBackgroundColor : Maybe Color -> { a | backgroundColor : Maybe Color } -> { a | backgroundColor : Maybe Color }
setBackgroundColor value node =
    { node | backgroundColor = value }


setBackground : Background -> { a | background : Background } -> { a | background : Background }
setBackground value node =
    { node | background = value }
