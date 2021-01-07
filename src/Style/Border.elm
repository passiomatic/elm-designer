module Style.Border exposing
    ( BorderCorner
    , BorderStyle(..)
    , BorderWidth
    , borderCorner
    , borderStyleName
    , borderWidth
    , setBorderBottomLeftCorner
    , setBorderBottomRightCorner
    , setBorderBottomWidth
    , setBorderColor
    , setBorderCorner
    , setBorderLeftWidth
    , setBorderRightWidth
    , setBorderStyle
    , setBorderTopLeftCorner
    , setBorderTopRightCorner
    , setBorderTopWidth
    , setBorderWidth
    )

{-| Border properties.
-}

import Element exposing (Color)


type alias BorderWidth =
    { locked : Bool
    , top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


borderWidth : Int -> BorderWidth
borderWidth value =
    BorderWidth True value value value value


setBorderWidth : BorderWidth -> { a | borderWidth : BorderWidth } -> { a | borderWidth : BorderWidth }
setBorderWidth value node =
    { node | borderWidth = value }


setBorderColor : Color -> { a | borderColor : Color } -> { a | borderColor : Color }
setBorderColor value node =
    { node | borderColor = value }


setBorderTopWidth : Int -> BorderWidth -> BorderWidth
setBorderTopWidth value borderWidth_ =
    if borderWidth_.locked then
        borderWidth value

    else
        { borderWidth_ | top = value }


setBorderRightWidth : Int -> BorderWidth -> BorderWidth
setBorderRightWidth value borderWidth_ =
    if borderWidth_.locked then
        borderWidth value

    else
        { borderWidth_ | right = value }


setBorderBottomWidth : Int -> BorderWidth -> BorderWidth
setBorderBottomWidth value borderWidth_ =
    if borderWidth_.locked then
        borderWidth value

    else
        { borderWidth_ | bottom = value }


setBorderLeftWidth : Int -> BorderWidth -> BorderWidth
setBorderLeftWidth value borderWidth_ =
    if borderWidth_.locked then
        borderWidth value

    else
        { borderWidth_ | left = value }


type BorderStyle
    = Solid
    | Dashed
    | Dotted


type alias BorderCorner =
    { locked : Bool
    , topLeft : Int
    , topRight : Int
    , bottomRight : Int
    , bottomLeft : Int
    }


borderCorner : Int -> BorderCorner
borderCorner value =
    BorderCorner True value value value value


setBorderCorner : BorderCorner -> { a | borderCorner : BorderCorner } -> { a | borderCorner : BorderCorner }
setBorderCorner value node =
    { node | borderCorner = value }


setBorderTopLeftCorner : Int -> BorderCorner -> BorderCorner
setBorderTopLeftCorner value borderCorner_ =
    if borderCorner_.locked then
        borderCorner value

    else
        { borderCorner_ | topLeft = value }


setBorderTopRightCorner : Int -> BorderCorner -> BorderCorner
setBorderTopRightCorner value borderCorner_ =
    if borderCorner_.locked then
        borderCorner value

    else
        { borderCorner_ | topRight = value }


setBorderBottomRightCorner : Int -> BorderCorner -> BorderCorner
setBorderBottomRightCorner value borderCorner_ =
    if borderCorner_.locked then
        borderCorner value

    else
        { borderCorner_ | bottomRight = value }


setBorderBottomLeftCorner : Int -> BorderCorner -> BorderCorner
setBorderBottomLeftCorner value borderCorner_ =
    if borderCorner_.locked then
        borderCorner value

    else
        { borderCorner_ | bottomLeft = value }


setBorderStyle : BorderStyle -> { a | borderStyle : BorderStyle } -> { a | borderStyle : BorderStyle }
setBorderStyle value node =
    { node | borderStyle = value }


borderStyleName : BorderStyle -> String
borderStyleName borderStyle =
    case borderStyle of
        Solid ->
            "Solid"

        Dashed ->
            "Dashed"

        Dotted ->
            "Dotted"
