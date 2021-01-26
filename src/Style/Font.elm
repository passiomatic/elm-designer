module Style.Font exposing
    ( FontFamily
    , FontType(..)
    , FontWeight(..)
    , Local(..)
    , TextAlignment(..)
    , minFontSizeAllowed
    , setFontColor
    , setFontFamily
    , setFontSize
    , setFontWeight
    , setLetterSpacing
    , setTextAlignment
    , weightName, setWordSpacing
    )

{-| Font properties.
-}

import Element exposing (Color)
import Element.Font exposing (letterSpacing)


minFontSizeAllowed =
    10


{-| A style value, specified locally or inherited from parent element.

    In Elm UI only `Font.color`, `Font.size`, and `Font.family` are inherited.

-}
type Local a
    = Local a
    | Inherit


type FontType
    = Native (List String)
    | External String


type alias FontFamily =
    { name : String
    , type_ : FontType
    , weights : List FontWeight
    }


setFontFamily : Local FontFamily -> { a | fontFamily : Local FontFamily } -> { a | fontFamily : Local FontFamily }
setFontFamily value node =
    { node | fontFamily = value }


setFontSize : Local Int -> { a | fontSize : Local Int } -> { a | fontSize : Local Int }
setFontSize value node =
    { node | fontSize = value }


setFontColor : Local Color -> { a | fontColor : Local Color } -> { a | fontColor : Local Color }
setFontColor value node =
    { node | fontColor = value }


setFontWeight : FontWeight -> { a | fontWeight : FontWeight } -> { a | fontWeight : FontWeight }
setFontWeight value node =
    { node | fontWeight = value }


setLetterSpacing : Float -> { b | letterSpacing : Float } -> { b | letterSpacing : Float }
setLetterSpacing value node =
    { node | letterSpacing = value }


setWordSpacing : Float -> { b | wordSpacing : Float } -> { b | wordSpacing : Float }
setWordSpacing value node =
    { node | wordSpacing = value }


{-| Unlike Elm UI a font weight tells more than how a font is lighter or darker.

    In fact, each font weight may have a paired italic version so here we expand the weight list to handle all cases.

-}
type FontWeight
    = Heavy
    | HeavyItalic
    | ExtraBold
    | ExtraBoldItalic
    | Bold
    | BoldItalic
    | SemiBold
    | SemiBoldItalic
    | Medium
    | MediumItalic
    | Regular
    | Italic
    | Light
    | LightItalic
    | ExtraLight
    | ExtraLightItalic
    | Hairline
    | HairlineItalic


{-| Prettified font weight names.
-}
weightName : FontWeight -> String
weightName value =
    case value of
        Heavy ->
            "Heavy"

        HeavyItalic ->
            "Heavy Italic"

        ExtraBold ->
            "Extra-bold"

        ExtraBoldItalic ->
            "Extra-bold Italic"

        Bold ->
            "Bold"

        BoldItalic ->
            "Bold Italic"

        SemiBold ->
            "Semi-bold"

        SemiBoldItalic ->
            "Semi-bold Italic"

        Medium ->
            "Medium"

        MediumItalic ->
            "Medium Italic"

        Regular ->
            "Regular"

        Italic ->
            "Italic"

        Light ->
            "Light"

        LightItalic ->
            "Light Italic"

        ExtraLight ->
            "Extra-light"

        ExtraLightItalic ->
            "Extra-light Italic"

        Hairline ->
            "Hairline"

        HairlineItalic ->
            "Hairline Italic"


type TextAlignment
    = TextLeft
    | TextCenter
    | TextRight
    | TextJustify


setTextAlignment : TextAlignment -> { a | textAlignment : TextAlignment } -> { a | textAlignment : TextAlignment }
setTextAlignment value node =
    { node | textAlignment = value }



-- type FontTransform
--     = Uppercase
--     | Lowercase
--     | Capitalize
--     | SmallCaps
