module Style.Shadow exposing
    ( Shadow
    , ShadowType(..)
    , none
    )

{-| Shadow properties.
-}

import Element exposing (Color)
import Palette


type ShadowType
    = Inner
    | Outer


type alias Shadow =
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Color
    , type_ : ShadowType
    }


none =
    Shadow ( 0, 0 ) 0 0 Palette.black Outer
