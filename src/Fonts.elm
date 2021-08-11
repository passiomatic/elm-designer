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

import Dict exposing (Dict)
import Style.Font
    exposing
        ( FontCategory(..)
        , FontFamily
        , FontType(..)
        , FontWeight(..)
        )


baseUrl =
    "https://fonts.googleapis.com/css2?family="


families : List ( String, List FontFamily )
families =
    [ ( "Native"
      , [ defaultFamily
        , FontFamily "-native-helvetica" "Helvetica" (Native helveticaFontStack) nativeWeights SansSerif
        , FontFamily "-native-georgia" "Georgia" (Native georgiaFontStack) nativeWeights Serif
        , FontFamily "-native-times-new-roman" "Times New Roman" (Native timesNewRomanFontStack) nativeWeights Serif
        ]
      )
    , ( "Sans Serif/Google Fonts"
      , [ FontFamily "alegreya-sans" "Alegreya Sans" (External alegreyaSansUrl) alegreyaSansWeights SansSerif
        , FontFamily "ibm-plex-sans" "IBM Plex Sans" (External plexSansUrl) plexSansWeights SansSerif
        , FontFamily "inter" "Inter" (External interUrl) interWeights SansSerif
        , FontFamily "roboto" "Roboto" (External robotoUrl) robotoWeights SansSerif
        , FontFamily "rubik" "Rubik" (External rubikUrl) rubikWeights SansSerif
        , FontFamily "source-sans-pro" "Source Sans Pro" (External sourceSansProUrl) sourceSansProWeights SansSerif
        , FontFamily "work-sans" "Work Sans" (External workSansUrl) workSansWeights SansSerif
        ]
      )
    , ( "Serif/Google Fonts"
      , [ FontFamily "alegreya" "Alegreya" (External alegreyaUrl) alegreyaWeights Serif
        , FontFamily "cormorant" "Cormorant" (External cormorantUrl) cormorantWeights Serif
        , FontFamily "eczar" "Eczar" (External eczarUrl) eczarWeights Serif
        , FontFamily "libre-baskerville" "Libre Baskerville" (External libreBaskervilleUrl) libreBaskervilleWeights Serif
        , FontFamily "lora" "Lora" (External loraUrl) loraWeights Serif
        , FontFamily "source-serif-pro" "Source Serif Pro" (External sourceSerifProUrl) sourceSerifProWeights Serif
        ]
      )
    , ( "Fixed-width/Google Fonts"
      , [ FontFamily "roboto-mono" "Roboto Mono" (External robotoMonoUrl) robotoMonoWeights Monospace
        , FontFamily "space-mono" "Space Mono" (External spaceMonoUrl) spaceMonoWeights Monospace
        , FontFamily "source-code-pro" "Source Code Pro" (External sourceCodeProUrl) sourceCodeProWeights Monospace
        ]
      )
    ]


{-| Default font families on startup.
-}
defaultFamilies : List String
defaultFamilies =
    -- Native
    [ "-native-system"
    , "-native-helvetica"
    , "-native-georgia"
    , "-native-times-new-roman"

    -- Sans Serif
    , "alegreya-sans"
    , "ibm-plex-sans"
    , "inter"
    , "roboto"
    , "rubik"
    , "source-sans-pro"
    , "work-sans"

    -- Serif
    , "alegreya"
    , "cormorant"
    , "eczar"
    , "libre-baskerville"
    , "lora"
    , "source-serif-pro"

    -- Monospace
    , "roboto-mono"
    , "space-mono"
    , "source-code-pro"
    ]


families_ : Dict String FontFamily
families_ =
    families
        |> List.map (\( _, f ) -> f)
        |> List.concat
        |> List.map (\family -> ( family.id, family ))
        |> Dict.fromList


findFamily : String -> FontFamily
findFamily id =
    Dict.get id families_
        |> Maybe.withDefault defaultFamily


defaultFamily =
    FontFamily "-native-system" "System" (Native systemFontStack) nativeWeights SansSerif


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


interUrl =
    baseUrl ++ "Inter:wght@100;200;300;400;500;600;700;800;900&display=swap"


interWeights : List FontWeight
interWeights =
    [ Hairline
    , ExtraLight
    , Light
    , Regular
    , Medium
    , Bold
    , SemiBold
    , ExtraBold
    , Heavy
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


plexSansUrl =
    baseUrl ++ "IBM+Plex+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&display=swap"


plexSansWeights =
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
    ]


sourceSansProUrl =
    baseUrl ++ "Source+Sans+Pro:ital,wght@0,200;0,300;0,400;0,600;0,700;0,900;1,200;1,300;1,400;1,600;1,700;1,900&display=swap"


sourceSansProWeights =
    [ ExtraLight
    , ExtraLightItalic
    , Light
    , LightItalic
    , Regular
    , Italic
    , SemiBold
    , SemiBoldItalic
    , Bold
    , BoldItalic
    , Heavy
    , HeavyItalic
    ]


alegreyaSansUrl =
    baseUrl ++ "Alegreya+Sans:ital,wght@0,100;0,300;0,400;0,500;0,700;0,800;0,900;1,100;1,300;1,400;1,500;1,700;1,800;1,900&display=swap"


alegreyaSansWeights =
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
    , ExtraBold
    , ExtraBoldItalic
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


loraUrl =
    baseUrl ++ "Lora:ital,wght@0,400;0,500;0,600;0,700;1,400;1,500;1,600;1,700&display=swap"


loraWeights =
    [ Regular
    , Italic
    , Medium
    , MediumItalic
    , SemiBold
    , SemiBoldItalic
    , Bold
    , BoldItalic
    ]


libreBaskervilleUrl =
    baseUrl ++ "Libre+Baskerville:ital,wght@0,400;0,700;1,400&display=swap"


libreBaskervilleWeights =
    [ Regular
    , Italic
    , Bold
    ]


sourceSerifProUrl =
    baseUrl ++ "Source+Serif+Pro:ital,wght@0,200;0,300;0,400;0,600;0,700;0,900;1,200;1,300;1,400;1,600;1,700;1,900&display=swap"


sourceSerifProWeights =
    [ ExtraLight
    , ExtraLightItalic
    , Light
    , LightItalic
    , Regular
    , Italic
    , SemiBold
    , SemiBoldItalic
    , Bold
    , BoldItalic
    , Heavy
    , HeavyItalic
    ]


spaceMonoUrl =
    baseUrl ++ "Space+Mono:ital,wght@0,400;0,700;1,400;1,700&display=swap"


spaceMonoWeights =
    [ Regular
    , Italic
    , Bold
    , BoldItalic
    ]


robotoMonoUrl =
    baseUrl ++ "Roboto+Mono:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&family=Space+Mono:ital,wght@0,400;0,700;1,400;1,700&display=swap"


robotoMonoWeights =
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
    ]


sourceCodeProUrl =
    baseUrl ++ "Source+Code+Pro:ital,wght@0,200;0,300;0,400;0,500;0,600;0,700;0,900;1,200;1,300;1,400;1,500;1,600;1,700;1,900&display=swap"


sourceCodeProWeights =
    [ ExtraLight
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
