module Style.Font exposing
    ( FontCategory(..)
    , FontFamily
    , FontType(..)
    , FontWeight(..)
    , Local(..)
    , TextAlignment(..)
    , findClosestWeight
    , isItalic
    , minFontSizeAllowed
    , setColor
    , setFamily
    , setLetterSpacing
    , setSize
    , setTextAlignment
    , setWeight
    , setWordSpacing
    , weightName, weightNumber
    )

{-| Font properties.
-}

import Element exposing (Color)


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


type FontCategory
    = Serif
    | SansSerif
    | Handwriting
    | Display
    | Monospace


type alias FontFamily =
    { id : String
    , name : String
    , type_ : FontType
    , weights : List FontWeight
    , category : FontCategory
    }


setFamily : Local FontFamily -> { a | fontFamily : Local FontFamily } -> { a | fontFamily : Local FontFamily }
setFamily value node =
    { node | fontFamily = value }


setSize : Local Int -> { a | fontSize : Local Int } -> { a | fontSize : Local Int }
setSize value node =
    { node | fontSize = value }


setColor : Local Color -> { a | fontColor : Local Color } -> { a | fontColor : Local Color }
setColor value node =
    { node | fontColor = value }


setWeight : FontWeight -> { a | fontWeight : FontWeight } -> { a | fontWeight : FontWeight }
setWeight value node =
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


findClosestWeight : FontWeight -> List FontWeight -> FontWeight
findClosestWeight optimal weights =
    weights
        |> List.map
            (\weight ->
                ( weight, abs (weightNumber weight - weightNumber optimal) )
            )
        |> List.sortBy Tuple.second
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault Regular


{-| Mapping between weight and CSS values.
-}
weightNumber : FontWeight -> Int
weightNumber value =
    case value of
        Heavy ->
            900

        HeavyItalic ->
            901

        ExtraBold ->
            800

        ExtraBoldItalic ->
            801

        Bold ->
            700

        BoldItalic ->
            701

        SemiBold ->
            600

        SemiBoldItalic ->
            601

        Medium ->
            500

        MediumItalic ->
            501

        Regular ->
            400

        Italic ->
            401

        Light ->
            300

        LightItalic ->
            301

        ExtraLight ->
            200

        ExtraLightItalic ->
            201

        Hairline ->
            100

        HairlineItalic ->
            101


isItalic : FontWeight -> Bool
isItalic value =
    remainderBy 100 (weightNumber value) == 1


type TextAlignment
    = TextCenter
    | TextStart
    | TextEnd
    | TextJustify


setTextAlignment : TextAlignment -> { a | textAlignment : TextAlignment } -> { a | textAlignment : TextAlignment }
setTextAlignment value node =
    { node | textAlignment = value }



-- type FontTransform
--     = Uppercase
--     | Lowercase
--     | Capitalize
--     | SmallCaps
