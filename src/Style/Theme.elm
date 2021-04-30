module Style.Theme exposing
    ( Theme
    , ThemeValue(..)
    , applyTextFontFamily
    , applyTextSize
    , defaultTheme
    , large
    , regular
    , small
    , xlarge
    , xsmall, applyTextFontWeight
    )

import Element exposing (Color)
import Fonts
import Palette
import Style.Border as Border exposing (..)
import Style.Font as Font exposing (..)
import Style.Layout as Layout exposing (..)


{-| A style value can either be defined in the theme or in the node.
-}
type ThemeValue a
    = ThemeValue (Theme -> a)
    | NodeValue a


type alias FontApperance =
    { fontFamily : Local FontFamily
    , fontColor : Local Color
    , fontSize : Local Int
    , fontWeight : FontWeight
    , letterSpacing : Float
    , wordSpacing : Float
    }


type alias BorderAppearance =
    { borderColor : Color
    , borderStyle : BorderStyle
    , borderWidth : BorderWidth
    , borderCorner : BorderCorner
    }


-- type Appearance a
--     = FromTheme String (Theme2 -> a) a
--     | FromNode a


{-| Theme variables are logically grouped like this:

    Page
        - textColor*
        - textSize*
        - textFontFamily*
        - textFontWeight*
        - backgroundColor*

    Headings
        - headingColor*
        - heading1Size*
        - heading2Size*
        - heading3Size*
        - headingFontFamily*
        - headingFontWeight*

    Brand Colors
        - primaryColor
        - accentColor
        - mutedColor

    Form elements
        - labelColor*
        - borderWidth*
        - borderColor*
        - borderCorner*

-}
type alias Theme =
    { name : String
    , textColor : Color
    , headingColor : Color
    , labelColor : Color
    , backgroundColor : Color
    , primaryColor : Color
    , accentColor : Color
    , mutedColor : Color
    , textFontFamily : FontFamily
    , headingFontFamily : FontFamily
    , textSize : Int
    , heading1Size : Int
    , heading2Size : Int
    , heading3Size : Int
    , textFontWeight : FontWeight
    , headingFontWeight : FontWeight
    , paragraphSpacing : Spacing
    , headingSpacing : Spacing
    , borderWidth : BorderWidth
    , borderColor : Color
    , borderCorner : BorderCorner
    }


-- type alias Theme2 =
--     { name : String
--     --, pagePackground: Background
--     , text: FontAppearance
--     , heading1: FontAppearance  
--     , heading2: FontAppearance  
--     , heading3: FontAppearance  
--     , label:  FontAppearance
--     , border: BorderAppearance  
--     , primaryColor: Color 
--     , accentColor: Color 
--     , mutedColor: Color
--     }

defaultTheme : Theme
defaultTheme =
    { name = "Default"
    , textColor = Palette.darkCharcoal
    , headingColor = Palette.darkCharcoal
    , labelColor = Palette.darkCharcoal
    , backgroundColor = Palette.white
    , primaryColor = Palette.blue
    , accentColor = Palette.orange
    , mutedColor = Palette.lightGray

    -- , infoColor = Palette.lightBlue
    -- , dangerColor = Palette.lightRed
    -- , warningColor = Palette.lightYellow
    -- , successColor = Palette.lightGreen
    , textSize = 16
    , heading1Size = 36
    , heading2Size = 24
    , heading3Size = 18
    , textFontFamily = Fonts.defaultFamily
    , headingFontFamily = Fonts.defaultFamily
    , textFontWeight = Regular
    , headingFontWeight = Bold
    , paragraphSpacing = Layout.spacingXY 0 (round (16 * 0.25))
    , headingSpacing = Layout.spacing 0
    , borderWidth = Border.width 1
    , borderColor = Palette.darkGray
    , borderCorner = Border.corner 2
    }


xsmall theme =
    theme.textSize // 4


small theme =
    theme.textSize // 2


regular theme =
    theme.textSize


large theme =
    theme.textSize + theme.textSize // 2


xlarge theme =
    theme.textSize * 3


applyTextSize : String -> Theme -> Theme
applyTextSize value theme =
    let
        value_ =
            case String.toInt value of
                Just v ->
                    clamp Font.minFontSizeAllowed 999 v

                Nothing ->
                    -- TODO do not hardcode default here
                    16
    in
    { theme | textSize = value_ }


applyTextFontFamily : FontFamily -> Theme -> Theme
applyTextFontFamily value theme =
    { theme | textFontFamily = value }


applyTextFontWeight : FontWeight -> Theme -> Theme
applyTextFontWeight value theme =
    { theme | textFontWeight = value }
