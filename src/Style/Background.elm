module Style.Background exposing
    ( Background(..)
    , setBackground
    )

{-| Background appearance properties.
-}

import Element exposing (Color)


type Background
    = Solid Color
    | Image String
    | None


setBackground : Background -> { a | background : Background } -> { a | background : Background }
setBackground value node =
    { node | background = value }
