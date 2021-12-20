module DragDropHelper exposing (addDroppedNode, getDroppedNode, setDragImage)

{-| Drag and drop helpers.
-}

import Document exposing (DragId(..), DropId(..), Node)
import Json.Decode as Decode exposing (Decoder, Value)
import Model exposing (..)
import Ports
import Style.Layout as Layout exposing (..)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UUID exposing (Seeds, UUID)


{-| Figure out _what_ user just dropped.
-}
getDroppedNode : Model -> DragId -> { x : Float, y : Float } -> ( Seeds, Maybe (Tree Node), Zipper Node )
getDroppedNode model dragId position =
    case dragId of
        Move node ->
            case Document.selectNodeWith node.id model.document.present of
                Just zipper ->
                    if model.isAltDown then
                        -- Duplicate node
                        let
                            ( newSeeds, newNode ) =
                                Document.duplicateNode zipper model.seeds
                        in
                        ( newSeeds, Just newNode, zipper )

                    else
                        -- Move node
                        let
                            newZipper =
                                Document.removeNode zipper
                        in
                        ( model.seeds, Just (Zipper.tree zipper), newZipper )

                Nothing ->
                    ( model.seeds, Nothing, model.document.present )

        Drag node ->
            case Document.selectNodeWith node.id model.document.present of
                Just zipper ->
                    let
                        -- Update node at new position
                        newNode =
                            Zipper.mapLabel
                                (\node_ ->
                                    { node_
                                        | offsetX = position.x
                                        , offsetY = position.y
                                    }
                                )
                                zipper
                                |> Zipper.tree

                        newZipper =
                            Document.removeNode zipper
                    in
                    ( model.seeds, Just newNode, newZipper )

                Nothing ->
                    ( model.seeds, Nothing, model.document.present )

        Insert node ->
            let
                ( newSeeds, newNode ) =
                    Document.fromTemplateAt position node model.seeds
            in
            ( newSeeds, Just newNode, model.document.present )


{-| Figure out _where_ user just dropped the node.
-}
addDroppedNode : Model -> DropId -> Tree Node -> Zipper Node -> Zipper Node
addDroppedNode model dropId node zipper =
    case dropId of
        -- Insert new element just before the sibling
        InsertBefore siblingId ->
            Document.insertNodeBefore siblingId node zipper

        -- Insert new element just after the sibling
        InsertAfter siblingId ->
            Document.insertNodeAfter siblingId node zipper

        -- Add new element as last child
        AppendTo parentId ->
            case Document.selectNodeWith parentId zipper of
                Just zipper_ ->
                    Document.appendNode node zipper_

                Nothing ->
                    zipper


setDragImage dragStart =
    case dragStart.dragId of
        Drag node ->
            let
                -- _ =
                --     Debug.log "DragEvent" (Decode.decodeValue dragEventDecoder dragStart.event)

                -- TODO Check node.widthMin as fallback
                width =
                    case node.width of
                        Px value ->
                            value

                        _ ->
                            999

                height =
                    case node.heightMin of
                        Just value ->
                            value

                        _ ->
                            999
            in
            Ports.setDragImage { event = dragStart.event, width = Just width, height = Just height }

        _ ->
            -- Use intrisct dimensions
            Ports.setDragImage { event = dragStart.event, width = Nothing, height = Nothing }


type alias DragEvent =
    { offsetX : Int
    , offsetY : Int
    }


dragEventDecoder : Decoder DragEvent
dragEventDecoder =
    Decode.map2 DragEvent
        (Decode.field "offsetX" Decode.float |> Decode.map round)
        (Decode.field "offsetY" Decode.float |> Decode.map round)
