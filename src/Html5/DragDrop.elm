module Html5.DragDrop exposing
    ( Model, init, Msg, update, updateSticky
    , draggable, droppable
    , getDragId, getDropId, getDroppablePosition
    , getDragstartEvent
    , DroppablePosition
    )

{-| This library handles dragging and dropping using the API
from the HTML 5 recommendation at
<https://www.w3.org/TR/html/editing.html#drag-and-drop>.

It provides attributes and a model/update to handle
dragging and dropping between your elements.

Types are parametrized with a `dragId` and a `dropId` parameter, which are the
types for the drag identifier passed to the [`draggable`](#draggable) function
and the drop identifier passed to the [`droppable`](#droppable) function.
You can put whatever data you like in these, but don't use function types.

You can use several instances of this model at the same time and they won't
interfere with each other. Drag and drop are connected to an instance by the
Msg constructor used, and the update function will not send a result if a drop
was made from another instance.

To use on mobile, you can include the following polyfill:
<https://github.com/Bernardo-Castilho/dragdroptouch>

Note that drag and drop _does not_ work out of the box in Firefox.
See the example folder in github for an example that uses ports
to do `event.dataTransfer.setData('text', '')`. to fix this.


# Model and update

@docs Model, init, Msg, Position, update, updateSticky


# View attributes

@docs draggable, droppable


# Status functions

@docs getDragId, getDropId, getDroppablePosition


# Javascript interop

@docs getDragstartEvent

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (Value)


{-| The drag and drop state.

This should be placed inside your application's model like this:

    type alias Model =
        { ...
        , dragDrop : Html5.DragDrop.Model DragId DropId
        }

-}
type Model dragId dropId
    = NotDragging
    | Dragging dragId DraggablePosition
    | DraggedOver dragId dropId Int DraggablePosition (Maybe DroppablePosition)


{-| The position inside a droppable. Contains the droppable's
width and height, as well as the current x and y position,
using the `currentTarget.clientWidth`, `currentTarget.clientHeight`, `offsetX`, and `offsetY`
from the `ondragover` event.

Note, that in some cases, x and y may be negative, or larger than the clientWidth and height,
if a drop event is registered outside the CSS padding edge.

-}
type alias DroppablePosition =
    { width : Int
    , height : Int
    , x : Int
    , y : Int
    , classes: String
    }


{-| The pointer position inside a draggable.
-}
type alias DraggablePosition =
    { offsetX : Int
    , offsetY : Int
    }


{-| The initial drag and drop state.

You should use this as the initital value for the drag and drop state in your model.

-}
init : Model dragId dropId
init =
    NotDragging


{-| The drag and drop messages.

This should be placed inside your application's messages like this:

    type Msg
        = ...
        | DragDropMsg (Html5.DragDrop.Msg DragId DropId)

-}
type Msg dragId dropId
    = DragStart dragId Json.Value
    | DragEnd
    | DragEnter dropId
    | DragLeave dropId
    | DragOver dropId Int DroppablePosition
    | Drop dropId DroppablePosition


{-| The update function.

When a successful drag and drop is made, this function will return a result
consisting of the `dragId` and `dropId` that was specified in the
[`draggable`](#draggable) and [`droppable`](#droppable)
calls for the corresponding nodes. It will also return a [`Position`](#Position)
for the drop event.

This should be placed inside your application's update function, like this:

    update msg model =
        case msg of
            ...
            DragDropMsg msg_ ->
                let
                    ( model_, result ) =
                        Html5.DragDrop.update msg_ model.dragDrop
                in
                    { model
                        | dragDrop = model_
                        , ...use result if available...
                    }

-}
update : Msg dragId dropId -> Model dragId dropId -> ( Model dragId dropId, Maybe ( dragId, dropId, DroppablePosition ) )
update =
    updateCommon False


{-| A "sticky" version of the [`update`](#update) function.

It's used the same way as the [`update`](#update) function, but when you use this version,
droppables are "sticky" so when you drag out of them and release the mouse button,
a drop will still be registered at the last droppable. You should preferably
provide some sort of indication (using [`getDropId`](#getDropId)) where the drop will take
place if you use this function.

-}
updateSticky : Msg dragId dropId -> Model dragId dropId -> ( Model dragId dropId, Maybe ( dragId, dropId, DroppablePosition ) )
updateSticky =
    updateCommon True


updateCommon :
    Bool
    -> Msg dragId dropId
    -> Model dragId dropId
    -> ( Model dragId dropId, Maybe ( dragId, dropId, DroppablePosition ) )
updateCommon sticky msg model =
    case ( msg, model, sticky ) of
        ( DragStart dragId event, _, _ ) ->
            let
                pos =
                    Json.decodeValue draggablePositionDecoder event
                        |> Result.toMaybe
                        |> Maybe.withDefault (DraggablePosition 0 0)
            in
            ( Dragging dragId pos, Nothing )

        ( DragEnd, _, _ ) ->
            ( NotDragging, Nothing )

        ( DragEnter dropId, Dragging dragId pos, _ ) ->
            -- let
            --     _ =
            --         Debug.log "DragEnter while Dragging" pos
            -- in
            ( DraggedOver dragId dropId 0 pos Nothing, Nothing )

        ( DragEnter dropId, DraggedOver dragId _ _ draggablePos droppablePos, _ ) ->
            -- let
            --     _ =
            --         Debug.log "DragEnter while DraggedOver" dropId
            -- in          
            ( DraggedOver dragId dropId 0 draggablePos droppablePos, Nothing )

        -- Only handle DragLeave if it is for the current dropId.
        -- DragLeave and DragEnter sometimes come in the wrong order
        -- when two droppables are next to each other.
        ( DragLeave dropId_, DraggedOver dragId dropId _ draggablePos _, False ) ->
            -- let
            --     _ =
            --         Debug.log "DragLeave while DraggedOver" dropId_
            -- in        
            if dropId_ == dropId then
                ( Dragging dragId draggablePos, Nothing )

            else
                ( model, Nothing )

        ( DragOver dropId timeStamp pos, Dragging dragId draggablePos, _ ) ->
            ( DraggedOver dragId dropId timeStamp draggablePos (Just pos), Nothing )

        ( DragOver dropId timeStamp pos, DraggedOver dragId _ currentTimeStamp draggablePos _, _ ) ->
            if timeStamp == currentTimeStamp then
                -- Handle dragover bubbling, if we already have handled this event
                -- (by looking at the timeStamp), do nothing. Also, this does some rate limiting
                -- if multiple events occur in the same time stamp.
                ( model, Nothing )

            else
                -- Update coordinates
                ( DraggedOver dragId dropId timeStamp draggablePos (Just pos), Nothing )

        ( Drop dropId pos, Dragging dragId draggablePos, _ ) ->
            let
                _ =
                    Debug.log "Drappable Pos.classes" pos.classes            
                pos_ =
                    getFinalPosition draggablePos pos
            in
            ( NotDragging, Just ( dragId, dropId, pos_ ) )

        ( Drop dropId pos, DraggedOver dragId _ _ draggablePos maybePos, _ ) ->
            let
                _ =
                    Debug.log "Drappable Pos.classes" pos.classes                
                pos_ =
                   getFinalPosition draggablePos pos
            in
            ( NotDragging, Just ( dragId, dropId, pos_ ) )

        _ ->
            ( model, Nothing )


getFinalPosition draggablePos pos =
    { pos
        | x = pos.x - draggablePos.offsetX
        , y = pos.y - draggablePos.offsetY
    }

{-| Attributes to make a node draggable.

The node you put these attributes on will be draggable with the `dragId` you provide.
It should be used like this:

    view =
       ...
       div (... ++ Html5.DragDrop.draggable DragDropMsg dragId) [...]

-}
draggable : (Msg dragId dropId -> msg) -> dragId -> List (Attribute msg)
draggable wrap drag =
    [ attribute "draggable" "true"
    , onWithOptions "dragstart" { stopPropagation = True, preventDefault = False } <| Json.map (wrap << DragStart drag) Json.value
    , onWithOptions "dragend" { stopPropagation = True, preventDefault = False } <| Json.succeed <| wrap <| DragEnd
    ]


{-| Attributes to make a node droppable.

The node you put these attributes on will be droppable with the `dropId` you provide.
It should be used like this:

    view =
       ...
       div (... ++ Html5.DragDrop.droppable DragDropMsg dropId) [...]

-}
droppable : (Msg dragId dropId -> msg) -> dropId -> List (Attribute msg)
droppable wrap dropId =
    [ onWithOptions "dragenter" { stopPropagation = True, preventDefault = True } <| Json.succeed <| wrap <| DragEnter dropId
    , onWithOptions "dragleave" { stopPropagation = True, preventDefault = True } <| Json.succeed <| wrap <| DragLeave dropId

    -- We don't stop propagation for dragover events because this will trigger redraw,
    -- and we get a lot of dragover events.
    , onWithOptions "dragover" { stopPropagation = False, preventDefault = True } <| Json.map wrap <| Json.map2 (DragOver dropId) timeStampDecoder droppablePositionDecoder
    , onWithOptions "drop" { stopPropagation = True, preventDefault = True } <| Json.map (wrap << Drop dropId) droppablePositionDecoder
    ]


timeStampDecoder : Json.Decoder Int
timeStampDecoder =
    Json.at [ "timeStamp" ] Json.float |> Json.map round


{-| Decode element offset within the droppable element, 
      recursively check parent nodes if needed.
  
  References:
    https://github.com/passiomatic/elm-designer/issues/45#issuecomment-1054397736
    https://www.quirksmode.org/js/findpos.html
    https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh
-}
droppablePositionDecoder : Json.Decoder DroppablePosition
droppablePositionDecoder =
    Json.map5 DroppablePosition
        (Json.at [ "currentTarget", "clientWidth" ] Json.int)
        (Json.at [ "currentTarget", "clientHeight" ] Json.int)
        (Json.at [ "offsetX" ] Json.float |> Json.map round)
        (Json.at [ "offsetY" ] Json.float |> Json.map round)
        (Json.at [ "target", "className" ] Json.string
            |> Json.andThen
                (\className ->
                    if String.contains "element--Document" className then
                        (Json.at [ "offsetX" ] Json.float |> Json.map round)

                    else
                        -- Check relative positioned parent   
                        Json.at [ "target", "offsetParent" ] Json.string
                )
        )


-- getOffsetFor node offsetX offsetY = 
--     if node.className == document then 
--         (Json.at [ "offsetX" ] Json.float |> Json.map round, Json.at [ "offsetY" ] Json.float |> Json.map round))
--          offsetX offsetY 
--     else if node.offsetParent then 
--         getOffsetFor node.offsetParent (offsetX + node.offsetLeft) (offsetY + node.offsetTop)
--     else 
--         (offsetX, offsetY)

type DomNode
    = DomNode { id : String, offsetParent : Value }

domNodeDecoder value accum = 
    Json.at [ "target", "className" ] Json.string
        |> Json.andThen
            (\className ->
                if String.contains "element--Document" className then
                    (Json.at [ "offsetX" ] Json.float |> Json.map round)

                else
                    -- Check relative positioned parent   
                    Json.at [ "target", "offsetParent" ] Json.string
            )    

{-| Decode pointer offset within the draggable element.
-}
draggablePositionDecoder : Json.Decoder DraggablePosition
draggablePositionDecoder =
    Json.map2 DraggablePosition
        (Json.at [ "offsetX" ] Json.float |> Json.map round)
        (Json.at [ "offsetY" ] Json.float |> Json.map round)


{-| Get the current `dragId` if available.

This function can be used for instance to hide the draggable when dragging.

-}
getDragId : Model dragId dropId -> Maybe dragId
getDragId model =
    case model of
        NotDragging ->
            Nothing

        Dragging dragId _ ->
            Just dragId

        DraggedOver dragId dropId _ _ _ ->
            Just dragId


{-| Get the current `dropId` if available.

This function can be used for instance to highlight the droppable when dragging over it.

Note that for efficiency reasons, the `dragover` event is being propagated,
so if you have a droppable inside another droppable you could get the wrong info
from `getDropId`. The package tries to ignore the extra events, but it may fail.

-}
getDropId : Model dragId dropId -> Maybe dropId
getDropId model =
    case model of
        NotDragging ->
            Nothing

        Dragging dragId _ ->
            Nothing

        DraggedOver dragId dropId _ _ _ ->
            Just dropId


{-| Get the current `Position` when dragging over the droppable.
-}
getDroppablePosition : Model dragId dropId -> Maybe DroppablePosition
getDroppablePosition model =
    case model of
        DraggedOver _ _ _ _ pos ->
            pos

        _ ->
            Nothing


{-| Get the `dragstart` event `Value` so that you can pass it to a port.
This is useful to fix Firefox behaviour. See the example directory in github
for how you can do that.

You can also use the event to do other things from Javascript,
such as setting the drag image.

-}
getDragstartEvent : Msg dragId dropId -> Maybe { dragId : dragId, event : Json.Value }
getDragstartEvent msg =
    case msg of
        DragStart dragId event ->
            Just { dragId = dragId, event = event }

        _ ->
            Nothing


{-| Polyfill for onWithOptions
-}
onWithOptions :
    String
    ->
        { stopPropagation : Bool
        , preventDefault : Bool
        }
    -> Json.Decoder msg
    -> Attribute msg
onWithOptions name { stopPropagation, preventDefault } decoder =
    decoder
        |> Json.map (\msg -> { message = msg, stopPropagation = stopPropagation, preventDefault = preventDefault })
        |> custom name
