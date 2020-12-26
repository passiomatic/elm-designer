module Palette exposing
    ( black
    , blue
    , brown
    , charcoal
    , darkBlue
    , darkBrown
    , darkCharcoal
    , darkGray
    , darkGreen
    , darkGrey
    , darkOrange
    , darkPurple
    , darkRed
    , darkYellow
    , gray
    , green
    , grey
    , lightBlue
    , lightBrown
    , lightCharcoal
    , lightGray
    , lightGreen
    , lightGrey
    , lightOrange
    , lightPurple
    , lightRed
    , lightYellow
    , orange
    , purple
    , red
    , transparent
    , white
    , yellow
    )

{-| These colors come from the Tango palette which provides aesthetically reasonable defaults for colors.

Each color also comes with a light and dark version.

Adapted from: <https://package.elm-lang.org/packages/avh4/elm-color/latest/Color>

-}

import Element exposing (Color)


lightRed : Color
lightRed =
    Element.rgba (239 / 255) (41 / 255) (41 / 255) 1.0


red : Color
red =
    Element.rgba (204 / 255) (0 / 255) (0 / 255) 1.0


darkRed : Color
darkRed =
    Element.rgba (164 / 255) (0 / 255) (0 / 255) 1.0


lightOrange : Color
lightOrange =
    Element.rgba (252 / 255) (175 / 255) (62 / 255) 1.0


orange : Color
orange =
    Element.rgba (245 / 255) (121 / 255) (0 / 255) 1.0


darkOrange : Color
darkOrange =
    Element.rgba (206 / 255) (92 / 255) (0 / 255) 1.0


lightYellow : Color
lightYellow =
    Element.rgba (255 / 255) (233 / 255) (79 / 255) 1.0


yellow : Color
yellow =
    Element.rgba (237 / 255) (212 / 255) (0 / 255) 1.0


darkYellow : Color
darkYellow =
    Element.rgba (196 / 255) (160 / 255) (0 / 255) 1.0


lightGreen : Color
lightGreen =
    Element.rgba (138 / 255) (226 / 255) (52 / 255) 1.0


green : Color
green =
    Element.rgba (115 / 255) (210 / 255) (22 / 255) 1.0


darkGreen : Color
darkGreen =
    Element.rgba (78 / 255) (154 / 255) (6 / 255) 1.0


lightBlue : Color
lightBlue =
    Element.rgba (114 / 255) (159 / 255) (207 / 255) 1.0


blue : Color
blue =
    Element.rgba (52 / 255) (101 / 255) (164 / 255) 1.0


darkBlue : Color
darkBlue =
    Element.rgba (32 / 255) (74 / 255) (135 / 255) 1.0


lightPurple : Color
lightPurple =
    Element.rgba (173 / 255) (127 / 255) (168 / 255) 1.0


purple : Color
purple =
    Element.rgba (117 / 255) (80 / 255) (123 / 255) 1.0


darkPurple : Color
darkPurple =
    Element.rgba (92 / 255) (53 / 255) (102 / 255) 1.0


lightBrown : Color
lightBrown =
    Element.rgba (233 / 255) (185 / 255) (110 / 255) 1.0


brown : Color
brown =
    Element.rgba (193 / 255) (125 / 255) (17 / 255) 1.0


darkBrown : Color
darkBrown =
    Element.rgba (143 / 255) (89 / 255) (2 / 255) 1.0


black : Color
black =
    Element.rgba (0 / 255) (0 / 255) (0 / 255) 1.0


white : Color
white =
    Element.rgba (255 / 255) (255 / 255) (255 / 255) 1.0


lightGrey : Color
lightGrey =
    Element.rgba (238 / 255) (238 / 255) (236 / 255) 1.0


grey : Color
grey =
    Element.rgba (211 / 255) (215 / 255) (207 / 255) 1.0


darkGrey : Color
darkGrey =
    Element.rgba (186 / 255) (189 / 255) (182 / 255) 1.0


lightGray : Color
lightGray =
    Element.rgba (238 / 255) (238 / 255) (236 / 255) 1.0


gray : Color
gray =
    Element.rgba (211 / 255) (215 / 255) (207 / 255) 1.0


darkGray : Color
darkGray =
    Element.rgba (186 / 255) (189 / 255) (182 / 255) 1.0


lightCharcoal : Color
lightCharcoal =
    Element.rgba (136 / 255) (138 / 255) (133 / 255) 1.0


charcoal : Color
charcoal =
    Element.rgba (85 / 255) (87 / 255) (83 / 255) 1.0


darkCharcoal : Color
darkCharcoal =
    Element.rgba (46 / 255) (52 / 255) (54 / 255) 1.0


transparent : Color
transparent =
    Element.rgba 1.0 1.0 1.0 0.0
