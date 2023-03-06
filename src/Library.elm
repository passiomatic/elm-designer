module Library exposing
    ( LibraryItem
    , findTemplate
    , groups
    , items
    )

{-
   All available elements in the app.

   While declaring a new library element there are certain Elm UI defaults,
   especially on widget widths, which makes sense to make explicit in the editor.

   See this Ellie: https://ellie-app.com/bQwRCwVkDvsa1
-}

import Dict
import Document exposing (..)
import Element as E exposing (Color)
import Html as H exposing (Html)
import Icons
import List.Extra
import Palette
import Style.Background as Background exposing (Background(..))
import Style.Font as Font exposing (..)
import Style.Input as Input exposing (LabelPosition(..))
import Style.Layout as Layout exposing (..)
import Style.Theme as Theme exposing (Theme)
import Tree as T exposing (Tree)


type alias LibraryItem msg =
    { root : Tree Node
    , icon : Html msg
    , group : String
    , description : String
    , accelerator : String
    }


basicsLabel =
    "Basics"


layoutLabel =
    "Layout"


formElementsLabel =
    "Form Elements"


items : List (LibraryItem msg)
items =
    [ heading1
    , heading2
    , heading3
    , textSnippet
    , paragraph
    , page
    , row
    , column
    , textColumn
    , button
    , buttonLight
    , buttonDark
    , textField
    , textFieldMultiline
    , checkbox
    , radio
    , option
    , slider
    ]
        |> List.map (\item -> item Theme.defaultTheme)


groups : List ( LibraryItem msg, List (LibraryItem msg) )
groups =
    List.Extra.gatherEqualsBy .group items


findTemplate : String -> Maybe (Tree Node)
findTemplate name =
    Dict.get name items_
        |> Maybe.map .root


items_ =
    items
        |> List.map
            (\item ->
                ( itemLabel item
                , item
                )
            )
        |> Dict.fromList


itemLabel =
    .root >> T.label >> .name



-- BASICS


heading1 : Theme -> LibraryItem msg
heading1 theme =
    { icon = Icons.type_
    , group = basicsLabel
    , description = ""
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | type_ = HeadingNode { level = 1, text = "" }
                , name = "Heading1"
                , width = Layout.fill
                , spacing = theme.headingSpacing
                , fontWeight = theme.headingFontWeight
                , fontSize = Local theme.heading1Size
                , fontColor = Local theme.headingColor
            }
    }


heading2 : Theme -> LibraryItem msg
heading2 theme =
    { icon = Icons.type_
    , group = basicsLabel
    , description = ""
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | type_ = HeadingNode { level = 2, text = "" }
                , name = "Heading2"
                , width = Layout.fill
                , spacing = theme.headingSpacing
                , fontWeight = theme.headingFontWeight
                , fontSize = Local theme.heading2Size
                , fontColor = Local theme.headingColor
            }
    }


heading3 : Theme -> LibraryItem msg
heading3 theme =
    { icon = Icons.type_
    , group = basicsLabel
    , description = ""
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | type_ = HeadingNode { level = 3, text = "" }
                , name = "Heading3"
                , width = Layout.fill
                , spacing = theme.headingSpacing
                , fontWeight = theme.headingFontWeight
                , fontSize = Local theme.heading3Size
                , fontColor = Local theme.headingColor
            }
    }


paragraph : Theme -> LibraryItem msg
paragraph theme =
    { icon = Icons.paragraph
    , group = basicsLabel
    , description = "Wrapping chunk of text"
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = "Paragraph"
                , width = Layout.fill
                , spacing = theme.paragraphSpacing
                , fontWeight = theme.textFontWeight
                , type_ = ParagraphNode { text = "" }
            }
    }


textSnippet : Theme -> LibraryItem msg
textSnippet theme =
    { icon = Icons.type_
    , group = basicsLabel
    , description = "Non-wrapping chunk of text"
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = "Text Snippet"
                , fontWeight = theme.textFontWeight
                , type_ = TextNode { text = "" }
            }
    }



-- LAYOUT


page : Theme -> LibraryItem msg
page theme =
    { icon = Icons.file
    , group = layoutLabel
    , description = ""
    , accelerator = ""
    , root =
        Document.emptyPage theme
    }


row : Theme -> LibraryItem msg
row theme =
    { icon = Icons.layout
    , group = layoutLabel
    , description = ""
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = "Row"
                , type_ = RowNode { wrapped = False }
                , width = Layout.fill
            }
    }


column : Theme -> LibraryItem msg
column theme =
    { icon = Icons.layout
    , group = layoutLabel
    , description = ""
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = "Column"
                , type_ = ColumnNode
                , width = Layout.fill
            }
    }


textColumn : Theme -> LibraryItem msg
textColumn theme =
    { icon = Icons.layout
    , group = layoutLabel
    , description = "Column made of paragraphs"
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = "Text Column"
                , type_ = TextColumnNode
                , width = Layout.fill
            }
    }



-- FORM ELEMENTS


textField : Theme -> LibraryItem msg
textField theme =
    { icon = Icons.edit
    , group = formElementsLabel
    , description = ""
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = "Text Field"
                , padding = Layout.padding (Theme.small theme)
                , spacing = Layout.spacingXY 0 (Theme.xsmall theme)
                , type_ =
                    TextFieldNode
                        { text = "Label"
                        , position = LabelAbove
                        , color = Inherited
                        }
                , width = Layout.fill
                , background = Background.solid theme.backgroundColor
                , borderWidth = theme.borderWidth
                , borderColor = theme.borderColor
                , borderCorner = theme.borderCorner
            }
    }


textFieldMultiline : Theme -> LibraryItem msg
textFieldMultiline theme =
    { icon = Icons.edit
    , group = formElementsLabel
    , description = "Resizes based on its contents"
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = "Multiline Field"
                , padding = Layout.padding (Theme.small theme)
                , spacing = Layout.spacingXY 0 (Theme.xsmall theme)
                , type_ =
                    TextFieldMultilineNode
                        { text = "Label"
                        , position = LabelAbove
                        , color = Inherited
                        }
                , width = Layout.fill
                , background = Background.solid theme.backgroundColor
                , borderWidth = theme.borderWidth
                , borderColor = theme.borderColor
                , borderCorner = theme.borderCorner
            }
    }


button : Theme -> LibraryItem msg
button theme =
    buttonHelper theme "Primary Button" theme.primaryColor theme.primaryColor


buttonLight : Theme -> LibraryItem msg
buttonLight theme =
    buttonHelper theme "Light Button" theme.borderColor Palette.white


buttonDark : Theme -> LibraryItem msg
buttonDark theme =
    buttonHelper theme "Dark Button" Palette.darkCharcoal Palette.darkCharcoal


buttonHelper : Theme -> String -> Color -> Color -> LibraryItem msg
buttonHelper theme name border background =
    { icon = Icons.coloredSquare border background
    , group = formElementsLabel
    , description = ""
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = name
                , padding = Layout.paddingXY (Theme.regular theme) (Theme.small theme)
                , borderWidth = theme.borderWidth
                , borderColor = border
                , borderCorner = theme.borderCorner
                , background = Solid background
                , fontColor = Local (Theme.contrastColor background theme.textColor Palette.white)
                , textAlignment = TextCenter
                , type_ = ButtonNode { text = "Button Label" }
            }
    }


checkbox : Theme -> LibraryItem msg
checkbox theme =
    { icon = Icons.checkSquare
    , group = formElementsLabel
    , description = ""
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = "Checkbox"
                , spacing = Layout.spacingXY (Theme.xsmall theme) 0
                , type_ =
                    CheckboxNode
                        { text = "Checkbox Label"
                        , position = LabelRight
                        , color = Inherited
                        }
            }
    }


radio : Theme -> LibraryItem msg
radio theme =
    { icon = Icons.checkCircle
    , group = formElementsLabel
    , description = ""
    , accelerator = ""
    , root =
        T.tree
            { baseTemplate
                | name = "Radio Selection"
                , spacing = Layout.spacingXY 0 (Theme.xsmall theme)
                , type_ =
                    RadioNode
                        { text = "Radio Selection"
                        , position = LabelAbove
                        , color = Inherited
                        }
            }
            [ T.singleton
                { baseTemplate
                    | name = "Option"
                    , type_ = OptionNode { text = "Option 1" }
                }
            , T.singleton
                { baseTemplate
                    | name = "Option"
                    , type_ = OptionNode { text = "Option 2" }
                }
            ]
    }


option : Theme -> LibraryItem msg
option theme =
    { icon = Icons.checkCircle
    , group = formElementsLabel
    , description = ""
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = "Radio Option"
                , type_ = OptionNode { text = "Option" }
            }
    }


slider : Theme -> LibraryItem msg
slider theme =
    { icon = Icons.sliders
    , group = formElementsLabel
    , description = ""
    , accelerator = ""
    , root =
        T.singleton
            { baseTemplate
                | name = "Slider"
                , height = Layout.Px 40
                , spacing = Layout.spacingXY 0 (Theme.xsmall theme)                
                , type_ = SliderNode { min = 0, max = 100, step = Nothing } (labelAbove "Label")
            }
    }



-- HELPERS


baseTemplate =
    Document.baseTemplate


labelAbove text =
    { text = text, position = LabelAbove, color = Inherited }


labelRight text =
    { text = text, position = LabelRight, color = Inherited }
