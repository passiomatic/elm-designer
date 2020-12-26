module Library exposing (LibraryItem, templates)

{-
   All available elements in the app.

   While declaring a new library element there are certain Elm UI defaults,
   especially on widget widths, which makes sense to make explicit in the editor.

   See this Ellie: https://ellie-app.com/bQwRCwVkDvsa1
-}

import Document exposing (..)
import Element as E exposing (Color)
import Fonts
import Html as H exposing (Html)
import Icons
import Palette
import Style.Font as Font exposing (..)
import Style.Layout as Layout exposing (..)
import Style.Theme as Theme exposing (Theme)
import Tree as T exposing (Tree)


type alias LibraryItem msg =
    { root : Tree Template
    , icon : Html msg
    , description : String
    }


templates : List ( String, List (LibraryItem msg) )
templates =
    let
        theme =
            Theme.defaultTheme
    in
    [ ( "Essentials"
      , [ LibraryItem (heading1 theme) Icons.type_ ""
        , LibraryItem (heading2 theme) Icons.type_ ""
        , LibraryItem (heading3 theme) Icons.type_ ""
        , LibraryItem (textSnippet theme) Icons.type_ "Non-wrapping chunk of text"
        , LibraryItem (paragraph theme) Icons.paragraph "Wrapping chunk of text"
        , LibraryItem (textColumn theme) Icons.layout "Column made of paragraphs"
        , LibraryItem (wrappedRow theme) Icons.layout ""
        , LibraryItem (column theme) Icons.layout ""
        ]
      )
    , ( "Form Elements"
      , [ LibraryItem (button theme) (Icons.coloredSquare theme.primaryColor theme.primaryColor) ""
        , LibraryItem (buttonLight theme) (Icons.coloredSquare Palette.darkCharcoal Palette.white) ""
        , LibraryItem (buttonDark theme) (Icons.coloredSquare Palette.darkCharcoal Palette.darkCharcoal) ""
        , LibraryItem (textField theme) Icons.edit ""
        , LibraryItem (textFieldMultiline theme) Icons.edit "Text field which resizes based on its contents"
        , LibraryItem (checkbox theme) Icons.checkSquare ""
        , LibraryItem (radio theme) Icons.checkCircle ""
        , LibraryItem (option theme) Icons.checkCircle ""
        ]
      )
    ]



-- ESSENTIALS


heading1 : Theme -> Tree Template
heading1 theme =
    T.singleton
        { baseTemplate
            | type_ = HeadingNode { level = 1, text = "" }
            , name = "Heading 1"
            , width = Fill
            , spacing = theme.headingSpacing
            , fontWeight = theme.headingFontWeight
            , fontSize = Local theme.heading1Size
            , fontColor = Local theme.headingColor
        }


heading2 : Theme -> Tree Template
heading2 theme =
    T.singleton
        { baseTemplate
            | type_ = HeadingNode { level = 2, text = "" }
            , name = "Heading 2"
            , width = Fill
            , spacing = theme.headingSpacing
            , fontWeight = theme.headingFontWeight
            , fontSize = Local theme.heading2Size
            , fontColor = Local theme.headingColor
        }


heading3 : Theme -> Tree Template
heading3 theme =
    T.singleton
        { baseTemplate
            | type_ = HeadingNode { level = 3, text = "" }
            , name = "Heading 3"
            , width = Fill
            , spacing = theme.headingSpacing
            , fontWeight = theme.headingFontWeight
            , fontSize = Local theme.heading3Size
            , fontColor = Local theme.headingColor
        }


paragraph : Theme -> Tree Template
paragraph theme =
    T.singleton
        { baseTemplate
            | name = "Paragraph"
            , width = Fill
            , spacing = theme.paragraphSpacing
            , fontWeight = theme.textFontWeight
            , type_ = ParagraphNode { text = "" }
        }


textSnippet : Theme -> Tree Template
textSnippet theme =
    T.singleton
        { baseTemplate
            | name = "Text Snippet"
            , fontWeight = theme.textFontWeight
            , type_ = TextNode { text = "" }
        }



-- LAYOUT


wrappedRow : Theme -> Tree Template
wrappedRow theme =
    T.singleton
        { baseTemplate
            | name = "Row"
            , type_ = RowNode { wrapped = True }
            , width = Fill
        }


column : Theme -> Tree Template
column theme =
    T.singleton
        { baseTemplate
            | name = "Column"
            , type_ = ColumnNode
            , width = Fill
        }


textColumn : Theme -> Tree Template
textColumn theme =
    T.singleton
        { baseTemplate
            | name = "Text Column"
            , type_ = TextColumnNode
            , width = Fill
        }



-- FORM ELEMENTS


textField : Theme -> Tree Template
textField theme =
    T.singleton
        { baseTemplate
            | name = "Text Field"
            , padding = Layout.padding (Theme.small theme)
            , spacing = Layout.spacingXY 0 (Theme.xsmall theme)
            , type_ =
                TextFieldNode
                    { text = "Label"
                    , position = LabelAbove
                    }
            , width = Fill
            , borderWidth = theme.borderWidth
            , borderColor = theme.borderColor
            , borderCorner = theme.borderCorner
        }


textFieldMultiline : Theme -> Tree Template
textFieldMultiline theme =
    T.singleton
        { baseTemplate
            | name = "Multiline Field"
            , padding = Layout.padding (Theme.small theme)
            , spacing = Layout.spacingXY 0 (Theme.xsmall theme)
            , type_ =
                TextFieldMultilineNode
                    { text = "Label"
                    , position = LabelAbove
                    }
            , width = Fill
            , borderWidth = theme.borderWidth
            , borderColor = theme.borderColor
            , borderCorner = theme.borderCorner
        }


button : Theme -> Tree Template
button theme =
    buttonHelper theme "Primary Button" theme.primaryColor theme.primaryColor


buttonLight : Theme -> Tree Template
buttonLight theme =
    buttonHelper theme "Light Button" theme.borderColor Palette.white


buttonDark : Theme -> Tree Template
buttonDark theme =
    buttonHelper theme "Dark Button" Palette.darkCharcoal Palette.darkCharcoal


buttonHelper : Theme -> String -> Color -> Color -> Tree Template
buttonHelper theme name border background =
    T.singleton
        { baseTemplate
            | name = name
            , padding = Layout.paddingXY (Theme.regular theme) (Theme.small theme)
            , borderWidth = theme.borderWidth
            , borderColor = border
            , borderCorner = theme.borderCorner
            , backgroundColor = Just background
            , fontColor = Local (contrastColor background theme.textColor Palette.white)
            , textAlignment = TextCenter
            , type_ = ButtonNode { text = "Button Label" }
        }


checkbox : Theme -> Tree Template
checkbox theme =
    T.singleton
        { baseTemplate
            | name = "Checkbox"
            , spacing = Layout.spacingXY (Theme.xsmall theme) 0
            , type_ = CheckboxNode { text = "Checkbox Label", position = LabelRight }
        }


radio : Theme -> Tree Template
radio theme =
    T.tree
        { baseTemplate
            | name = "Radio Selection"
            , spacing = Layout.spacingXY 0 (Theme.xsmall theme)
            , type_ = RadioNode { text = "Radio Selection", position = LabelRight }
        }
        [ T.singleton
            { baseTemplate
                | name = "Option 1"
                , type_ = OptionNode { text = "Option 1" }
            }
        , T.singleton
            { baseTemplate
                | name = "Option 2"
                , type_ = OptionNode { text = "Option 2" }
            }
        ]


option : Theme -> Tree Template
option theme =
    T.singleton
        { baseTemplate
            | name = "Radio Option"
            , type_ = OptionNode { text = "Option" }
        }


baseTemplate =
    Document.baseTemplate


{-| See <https://24ways.org/2010/calculating-color-contrast/>
-}
contrastColor color dark light =
    let
        { red, green, blue, alpha } =
            E.toRgb color
    in
    if ((red * 255 * 299) + (green * 255 * 587) + (blue * 255 * 114)) / 1000 > 150 then
        dark

    else
        light
