module Style.Border exposing
    ( BorderCorner
    , BorderStyle(..)
    , BorderWidth
    , corner
    , hasWidth
    , isDashed
    , isDotted
    , isRounded
    , isSolid
    , setBottomLeftCorner
    , setBottomRightCorner
    , setBottomWidth
    , setColor
    , setCorner
    , setLeftWidth
    , setRightWidth
    , setStyle
    , setTopLeftCorner
    , setTopRightCorner
    , setTopWidth
    , setWidth
    , width
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


width : Int -> BorderWidth
width value =
    BorderWidth True value value value value


hasWidth : BorderWidth -> Bool
hasWidth value =
    value.top /= 0 || value.right /= 0 || value.bottom /= 0 || value.left /= 0


setWidth : BorderWidth -> { a | borderWidth : BorderWidth } -> { a | borderWidth : BorderWidth }
setWidth value node =
    { node | borderWidth = value }


setColor : Color -> { a | borderColor : Color } -> { a | borderColor : Color }
setColor value node =
    { node | borderColor = value }


setTopWidth : Int -> BorderWidth -> BorderWidth
setTopWidth value record =
    if record.locked then
        width value

    else
        { record | top = value }


setRightWidth : Int -> BorderWidth -> BorderWidth
setRightWidth value record =
    if record.locked then
        width value

    else
        { record | right = value }


setBottomWidth : Int -> BorderWidth -> BorderWidth
setBottomWidth value record =
    if record.locked then
        width value

    else
        { record | bottom = value }


setLeftWidth : Int -> BorderWidth -> BorderWidth
setLeftWidth value record =
    if record.locked then
        width value

    else
        { record | left = value }


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


corner : Int -> BorderCorner
corner value =
    BorderCorner True value value value value


setCorner : BorderCorner -> { a | borderCorner : BorderCorner } -> { a | borderCorner : BorderCorner }
setCorner value node =
    { node | borderCorner = value }


setStyle : BorderStyle -> { a | borderStyle : BorderStyle } -> { a | borderStyle : BorderStyle }
setStyle value node =
    { node | borderStyle = value }


setTopLeftCorner : Int -> BorderCorner -> BorderCorner
setTopLeftCorner value record =
    if record.locked then
        corner value

    else
        { record | topLeft = value }


setTopRightCorner : Int -> BorderCorner -> BorderCorner
setTopRightCorner value record =
    if record.locked then
        corner value

    else
        { record | topRight = value }


setBottomRightCorner : Int -> BorderCorner -> BorderCorner
setBottomRightCorner value record =
    if record.locked then
        corner value

    else
        { record | bottomRight = value }


setBottomLeftCorner : Int -> BorderCorner -> BorderCorner
setBottomLeftCorner value record =
    if record.locked then
        corner value

    else
        { record | bottomLeft = value }


isRounded : BorderCorner -> Bool
isRounded value =
    value.topLeft /= 0 || value.topRight /= 0 || value.bottomLeft /= 0 || value.bottomRight /= 0


isSolid value =
    case value of
        Solid ->
            True

        _ ->
            False


isDashed value =
    case value of
        Dashed ->
            True

        _ ->
            False


isDotted value =
    case value of
        Dotted ->
            True

        _ ->
            False
