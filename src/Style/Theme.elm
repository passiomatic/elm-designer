module Style.Theme exposing
    ( Theme
    , defaultTheme
    , large
    , regular
    , small
    , xlarge
    , xsmall
    )

import Element exposing (Color)
import Fonts
import Palette
import Style.Border as Border exposing (..)
import Style.Font as Font exposing (..)
import Style.Layout as Layout exposing (..)


type alias Theme =
    { textColor : Color
    , headingColor : Color
    , labelColor : Color
    , backgroundColor : Color
    , primaryColor : Color
    , accentColor : Color
    , mutedColor : Color
    , infoColor : Color
    , dangerColor : Color
    , warningColor : Color
    , successColor : Color
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


defaultTheme : Theme
defaultTheme =
    { textColor = Palette.darkCharcoal
    , headingColor = Palette.darkCharcoal
    , labelColor = Palette.darkCharcoal
    , backgroundColor = Palette.white
    , primaryColor = Palette.blue
    , accentColor = Palette.orange
    , mutedColor = Palette.lightGray
    , infoColor = Palette.lightBlue
    , dangerColor = Palette.lightRed
    , warningColor = Palette.lightYellow
    , successColor = Palette.lightGreen
    , textSize = 16
    , heading1Size = 36
    , heading2Size = 24
    , heading3Size = 18
    , textFontFamily = Fonts.defaultFamily
    , headingFontFamily = Fonts.defaultFamily
    , textFontWeight = Regular
    , headingFontWeight = Bold
    , paragraphSpacing = Layout.spacingXY 0 (round (16 * 0.5))
    , headingSpacing = Layout.spacing 0
    , borderWidth = Border.borderWidth 1
    , borderColor = Palette.darkGray
    , borderCorner = Border.borderCorner 2
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
