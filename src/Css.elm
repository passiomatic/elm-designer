module Css exposing
    ( colorToString
    , colorToStringWithHash
    , em
    , percent
    , px
    , scaleBy
    , stringToColor
    , translateBy
    )

{-| CSS properties-to-string helpers.
-}

import Bitwise
import Element exposing (Color)
import Hex
import Html exposing (Attribute)
import Html.Attributes as A



-- TRANSFORM


translateBy x y =
    -- TODO use translate3d
    "translate(" ++ String.fromInt x ++ "px, " ++ String.fromInt y ++ "px)"


scaleBy value =
    "scale(" ++ String.fromFloat value ++ ")"



-- UNITS


px value =
    String.fromInt value ++ "px"


em value =
    String.fromFloat value ++ "em"


percent value =
    String.fromFloat value ++ "%"



-- COLOR


colorToString : Color -> String
colorToString value =
    colorToString_ False value


colorToStringWithHash =
    colorToString_ True


colorToString_ : Bool -> Color -> String
colorToString_ addHash value =
    let
        floatTo256 f =
            if f >= 1 then
                255

            else
                floor (f * 256)

        converter =
            floatTo256 >> Hex.toString >> String.padLeft 2 '0'

        rgba =
            Element.toRgb value
    in
    (if addHash then
        "#"

     else
        ""
    )
        ++ converter rgba.red
        ++ converter rgba.green
        ++ converter rgba.blue


stringToColor : String -> Color
stringToColor value =
    let
        rgb =
            -- Drop #
            (if String.startsWith "#" value then
                String.dropLeft 1 value

             else
                value
            )
                |> String.toLower
                |> Hex.fromString
                |> Result.withDefault 0

        r =
            Bitwise.shiftRightBy 16 rgb
                |> Bitwise.and 0xFF

        g =
            Bitwise.shiftRightBy 8 rgb
                |> Bitwise.and 0xFF

        b =
            Bitwise.and 0xFF rgb
    in
    Element.fromRgb255
        { red = r
        , green = g
        , blue = b
        , alpha = 1.0
        }
