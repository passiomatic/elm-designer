module Style.Layout exposing
    ( Alignment(..)
    , Length(..)
    , Padding
    , Spacing(..)
    , Transformation
    , fill
    , fit
    , padding
    , paddingXY
    , portion
    , px
    , setLock
    , setOffsetX
    , setOffsetY
    , setPadding
    , setPaddingBottom
    , setPaddingLeft
    , setPaddingRight
    , setPaddingTop
    , setSpacing
    , setSpacingX
    , setSpacingY
    , setTransformation
    , setWidthMax
    , setWidthMin
    , spacing
    , spacingXY
    , unspecified
    , untransformed, Position(..)
    )

{-| These types mirrors the Elm UI ones as much as possible.

    The idea is to not reinvent another vocabulary to describe the UI
    elements but to stick to what we are going to serialize in the end.

    We basically reconstruct several Elm UI opaque types, thus allowing
    to "case-of" on those.

-}


{-| Element length, used as width or height.
-}
type Length
    = Px Int
    | Content
    | Fill Int
    | Unspecified


px : Int -> Length
px value =
    Px value


fit : Length
fit =
    Content


fill : Length
fill =
    Fill 1


portion : Int -> Length
portion value =
    Fill value


unspecified =
    Unspecified


setWidthMin : Maybe Int -> { a | widthMin : Maybe Int } -> { a | widthMin : Maybe Int }
setWidthMin value node =
    { node | widthMin = value }


setWidthMax : Maybe Int -> { a | widthMax : Maybe Int } -> { a | widthMax : Maybe Int }
setWidthMax value node =
    { node | widthMax = value }



-- POSITION


type Position
    = Above
    | Below
    | OnStart
    | OnEnd
    | InFront
    | BehindContent
    | Normal


-- ALIGNMENT


type Alignment
    = Center
    | Start
    | End
    | None



-- PADDING


type alias Padding =
    { locked : Bool
    , top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


{-| Setup a locked padding value.
-}
padding : Int -> Padding
padding value =
    Padding True value value value value


{-| Setup a unlocked padding value.
-}
paddingXY : Int -> Int -> Padding
paddingXY x y =
    Padding False y x y x


setPadding : Padding -> { a | padding : Padding } -> { a | padding : Padding }
setPadding value node =
    { node | padding = value }


setPaddingTop : Int -> Padding -> Padding
setPaddingTop value padding_ =
    if padding_.locked then
        padding value

    else
        { padding_ | top = value }


setPaddingRight : Int -> Padding -> Padding
setPaddingRight value padding_ =
    if padding_.locked then
        padding value

    else
        { padding_ | right = value }


setPaddingBottom : Int -> Padding -> Padding
setPaddingBottom value padding_ =
    if padding_.locked then
        padding value

    else
        { padding_ | bottom = value }


setPaddingLeft : Int -> Padding -> Padding
setPaddingLeft value padding_ =
    if padding_.locked then
        padding value

    else
        { padding_ | left = value }



-- TRANSFORMATION


type alias Transformation =
    { offsetX : Float
    , offsetY : Float
    , rotation : Float
    , scale : Float
    }


{-| Create an "untransformed" style.
-}
untransformed =
    Transformation 0 0 0 1.0


setTransformation : Transformation -> { a | transformation : Transformation } -> { a | transformation : Transformation }
setTransformation value node =
    { node | transformation = value }


setOffsetX : Float -> Transformation -> Transformation
setOffsetX value transformation_ =
    { transformation_ | offsetX = value }


setOffsetY : Float -> Transformation -> Transformation
setOffsetY value transformation_ =
    { transformation_ | offsetY = value }



-- SPACING


type Spacing
    = SpaceEvenly
    | Spacing ( Int, Int )


spacing : Int -> Spacing
spacing value =
    Spacing ( value, value )


spacingXY : Int -> Int -> Spacing
spacingXY x y =
    Spacing ( x, y )


setSpacing : Spacing -> { a | spacing : Spacing } -> { a | spacing : Spacing }
setSpacing value node =
    { node | spacing = value }


setSpacingX : Int -> Spacing -> Spacing
setSpacingX value spacing_ =
    case spacing_ of
        Spacing ( _, y ) ->
            spacingXY value y

        SpaceEvenly ->
            spacing value


setSpacingY : Int -> Spacing -> Spacing
setSpacingY value spacing_ =
    case spacing_ of
        Spacing ( x, _ ) ->
            spacingXY x value

        SpaceEvenly ->
            spacing value



-- MISC


{-| Set lock flag for padding and borders.
-}
setLock : Bool -> { a | locked : Bool } -> { a | locked : Bool }
setLock value record =
    { record | locked = value }
