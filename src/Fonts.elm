module Fonts exposing
    ( defaultFamily
    , families
    , findFamily
    , links
    )

{-| All the available fonts in the app.

The list includes both native font stacks and a hand-picked list of typefaces available from Google Fonts.

References:

    * <https://www.typewolf.com/google-fonts>
    * <https://medium.com/skyscanner-design/a-native-font-stack-d9d0db72d6e6>
    * <https://gist.github.com/don1138/5761014>
    * <https://www.cssfontstack.com>

-}

--import Element.Font as Font exposing (Font)

import Dict exposing (Dict)
import SelectList exposing (SelectList)
import Style.Font
    exposing
        ( FontFamily
        , FontType(..)
        , FontWeight(..)
        )


baseUrl =
    "https://fonts.googleapis.com/css2?family="


families_ =
    families
        |> List.map (\family -> ( family.name, family ))
        |> Dict.fromList


families : List FontFamily
families =
    [ defaultFamily
    , FontFamily "Helvetica" (Native helveticaFontStack) nativeWeights
    , FontFamily "Times New Roman" (Native timesNewRomanFontStack) nativeWeights
    , FontFamily "Georgia" (Native georgiaFontStack) nativeWeights

    -- Sans serif
    , FontFamily "Work Sans" (External workSansUrl) workSansWeights
    , FontFamily "Rubik" (External rubikUrl) rubikWeights
    , FontFamily "Roboto" (External robotoUrl) robotoWeights

    -- Serif
    , FontFamily "Cormorant" (External cormorantUrl) cormorantWeights
    , FontFamily "Eczar" (External eczarUrl) eczarWeights
    , FontFamily "Alegreya" (External alegreyaUrl) alegreyaWeights
    ]


findFamily : String -> FontFamily
findFamily name =
    Dict.get name families_
        |> Maybe.withDefault defaultFamily


defaultFamily =
    FontFamily "System" (Native systemFontStack) nativeWeights


links : List String
links =
    Dict.foldl
        (\_ family accum ->
            case family.type_ of
                Native _ ->
                    accum

                External url ->
                    url :: accum
        )
        []
        families_


nativeWeights =
    [ Regular
    , Italic
    , Bold
    , BoldItalic
    ]



-- GOOGLE FONTS


cormorantUrl =
    baseUrl ++ "Cormorant:ital,wght@0,300;0,400;0,500;0,600;0,700;1,300;1,400;1,500;1,600;1,700&family=Work+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap"


cormorantWeights : List FontWeight
cormorantWeights =
    [ Light
    , LightItalic
    , Regular
    , Italic
    , Medium
    , MediumItalic
    , SemiBold
    , SemiBoldItalic
    , Bold
    , BoldItalic
    ]


workSansUrl =
    baseUrl ++ "Work+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap"


workSansWeights : List FontWeight
workSansWeights =
    [ Hairline
    , HairlineItalic
    , ExtraLight
    , ExtraLightItalic
    , Light
    , LightItalic
    , Regular
    , Italic
    , Medium
    , MediumItalic
    , SemiBold
    , SemiBoldItalic
    , Bold
    , BoldItalic
    , ExtraBold
    , ExtraBoldItalic
    , Heavy
    , HeavyItalic
    ]


rubikUrl =
    baseUrl ++ "Rubik:ital,wght@0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap"


rubikWeights : List FontWeight
rubikWeights =
    [ Light
    , LightItalic
    , Regular
    , Italic
    , Medium
    , MediumItalic
    , SemiBold
    , SemiBoldItalic
    , Bold
    , BoldItalic
    , ExtraBold
    , ExtraBoldItalic
    , Heavy
    , HeavyItalic
    ]


robotoUrl =
    baseUrl ++ "Roboto:ital,wght@0,100;0,300;0,400;0,500;0,700;0,900;1,100;1,300;1,400;1,500;1,700;1,900&display=swap"


robotoWeights =
    [ Hairline
    , HairlineItalic
    , Light
    , LightItalic
    , Regular
    , Italic
    , Medium
    , MediumItalic
    , Bold
    , BoldItalic
    , Heavy
    , HeavyItalic
    ]


eczarUrl =
    baseUrl ++ "Eczar:wght@400;500;600;700;800&display=swap"


eczarWeights =
    [ Regular
    , Medium
    , SemiBold
    , Bold
    , ExtraBold
    ]


alegreyaUrl =
    baseUrl ++ "Alegreya:ital,wght@0,400;0,500;0,700;0,800;0,900;1,400;1,500;1,700;1,800;1,900&display=swap"


alegreyaWeights =
    [ Regular
    , Italic
    , Medium
    , MediumItalic
    , Bold
    , BoldItalic
    , ExtraBold
    , ExtraBoldItalic
    , Heavy
    , HeavyItalic
    ]



-- NATIVE


{-| System UI fonts
-}
systemFontStack : List String
systemFontStack =
    [ "system-ui" -- CSS standard
    , "-apple-system" -- Older Safari/Firefox

    -- , "Roboto" -- Android
    -- , "Oxygen" -- KDE
    -- , "Ubuntu" -- Ubuntu
    -- , "Cantarell" -- Gnome
    , "sans-serif"
    ]


{-| The Helvetica/Arial-based sans serif
-}
helveticaFontStack : List String
helveticaFontStack =
    [ "Helvetica Neue"
    , "Helvetica"
    , "Arial"
    , "sans-serif"
    ]


{-| Times New Roman-based serif
-}
timesNewRomanFontStack : List String
timesNewRomanFontStack =
    [ "Cambria"
    , "Times"
    , "Times New Roman"
    , "serif"
    ]


{-| A modern Georgia-based serif
-}
georgiaFontStack : List String
georgiaFontStack =
    [ "Constantia"
    , "Georgia"
    , "serif"
    ]
