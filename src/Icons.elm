module Icons exposing
    ( alertTriangle
    , alignCenter
    , alignJustify
    , alignLeft
    , alignRight
    , arrowDown
    , arrowLeft
    , arrowRight
    , arrowUp
    , checkCircle
    , checkSquare
    , chevronDown
    , chevronRight
    , coloredSquare
    , edit
    , file
    , image
    , info
    , layout
    , lock
    , minusCircle
    , paragraph
    , pipe
    , play
    , plusCircle
    , plusCircleSmall
    , smartphone
    , square
    , stop
    , tablet
    , type_
    , unlock
    , xCircle
    )

import Css
import Element exposing (Color)
import FeatherIcons as I
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A


smallSize =
    14


mediumSize =
    16


largeSize =
    18


file : Html msg
file =
    I.file
        |> I.withSize smallSize
        |> I.toHtml []


image : Html msg
image =
    I.image
        |> I.withSize smallSize
        |> I.toHtml []


arrowUp : Html msg
arrowUp =
    I.arrowUp
        |> I.withSize largeSize
        |> I.toHtml []


arrowDown : Html msg
arrowDown =
    I.arrowDown
        |> I.withSize largeSize
        |> I.toHtml []


arrowLeft : Html msg
arrowLeft =
    I.arrowLeft
        |> I.withSize largeSize
        |> I.toHtml []


arrowRight : Html msg
arrowRight =
    I.arrowRight
        |> I.withSize largeSize
        |> I.toHtml []


square : Html msg
square =
    I.square
        |> I.withSize smallSize
        |> I.toHtml []


coloredSquare : Color -> Color -> Html msg
coloredSquare stroke fill =
    I.square
        |> I.withSize smallSize
        |> I.toHtml [ A.stroke (Css.colorToStringWithHash stroke), A.fill (Css.colorToStringWithHash fill) ]


layout : Html msg
layout =
    I.layout
        |> I.withSize smallSize
        |> I.toHtml []


play : Html msg
play =
    I.play
        |> I.withSize largeSize
        |> I.toHtml []


stop : Html msg
stop =
    square


smartphone : Html msg
smartphone =
    I.smartphone
        |> I.withSize largeSize
        |> I.toHtml []


tablet : Html msg
tablet =
    I.tablet
        |> I.withSize largeSize
        |> I.toHtml []



-- iMac : Html msg
-- iMac =
--     I.monitor
--         |> I.withSize largeSize
--         |> I.toHtml []


xCircle : Html msg
xCircle =
    I.xCircle
        |> I.withSize smallSize
        |> I.toHtml []


plusCircle : Html msg
plusCircle =
    I.plusCircle
        |> I.withSize largeSize
        |> I.toHtml []


plusCircleSmall : Html msg
plusCircleSmall =
    I.plusCircle
        |> I.withSize smallSize
        |> I.toHtml []


minusCircle : Html msg
minusCircle =
    I.minusCircle
        |> I.withSize largeSize
        |> I.toHtml []


lock : Html msg
lock =
    I.lock
        |> I.withSize mediumSize
        |> I.toHtml []


unlock : Html msg
unlock =
    I.unlock
        |> I.withSize mediumSize
        |> I.toHtml []


chevronRight : Html msg
chevronRight =
    I.chevronRight
        |> I.withSize smallSize
        |> I.toHtml []


chevronDown : Html msg
chevronDown =
    I.chevronDown
        |> I.withSize smallSize
        |> I.toHtml []


alertTriangle : Html msg
alertTriangle =
    I.alertTriangle
        |> I.withSize largeSize
        |> I.toHtml []


info : Html msg
info =
    I.info
        |> I.withSize largeSize
        |> I.toHtml []


pipe : Html msg
pipe =
    [ Svg.line [ A.x1 "12", A.y1 "19", A.x2 "12", A.y2 "5" ] []
    ]
        |> I.customIcon
        |> I.withSize 18
        |> I.withStrokeWidth 3
        |> I.withViewBox "0 0 24 24"
        |> I.toHtml []



-- FORMS


checkSquare : Html msg
checkSquare =
    I.checkSquare
        |> I.withSize smallSize
        |> I.toHtml []


checkCircle : Html msg
checkCircle =
    I.checkCircle
        |> I.withSize smallSize
        |> I.toHtml []


edit : Html msg
edit =
    I.edit
        |> I.withSize smallSize
        |> I.toHtml []



-- TEXT


type_ : Html msg
type_ =
    I.type_
        |> I.withSize smallSize
        |> I.toHtml []


paragraph : Html msg
paragraph =
    [ Svg.line [ A.x1 "21", A.y1 "5", A.x2 "3", A.y2 "5" ] []
    , Svg.line [ A.x1 "21", A.y1 "10", A.x2 "3", A.y2 "10" ] []
    , Svg.line [ A.x1 "16", A.y1 "15", A.x2 "3", A.y2 "15" ] []
    ]
        |> I.customIcon
        |> I.withSize smallSize
        |> I.withViewBox "0 0 24 24"
        |> I.toHtml []


alignLeft : Html msg
alignLeft =
    I.alignLeft
        |> I.withSize largeSize
        |> I.toHtml []


alignRight : Html msg
alignRight =
    I.alignRight
        |> I.withSize largeSize
        |> I.toHtml []


alignCenter : Html msg
alignCenter =
    I.alignCenter
        |> I.withSize largeSize
        |> I.toHtml []


alignJustify : Html msg
alignJustify =
    I.alignJustify
        |> I.withSize largeSize
        |> I.toHtml []
