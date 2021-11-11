module Views.UserModel exposing (view)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Module as Module exposing (Module(..))
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAlias as TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation(..))
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import Html5.DragDrop as DragDrop
import Icons
import Json.Decode as Decode exposing (Decoder)
import Model exposing (..)
import Palette
import Set exposing (Set)


basics : Set String
basics =
    [ "Int"
    , "Float"
    , "String"
    , "List"
    , "Dict"
    , "Set"
    ]
        |> Set.fromList


view : Model -> Html Msg
view model =
    let
        types =
            Dict.foldl userTypeView [] model.types
    in
    H.div [ A.class "bp-3 scroll-y", A.style "height" "350px", A.style "min-height" "350px" ]
        (header :: types)


header =
    H.div [ A.class "fw-500 mb-3" ]
        [ H.text "Model" ]


userTypeView name userType accum =
    case Node.value userType of
        AliasDeclaration alias_ ->
            H.div [ A.class "border rounded mb-3" ]
                [ H.table [ A.class "table table-striped table-borderless table-user-model mb-0" ]
                    [ H.caption [ A.style "caption-side" "top", A.class "section__title bpx-1 bpy-2" ] [ H.text ("type alias " ++ name) ]
                    , H.thead [ A.class "border-end border-start" ]
                        [ H.tr []
                            [ H.th [ A.class "w-33" ]
                                [ H.text "Field"
                                ]
                            , H.th []
                                [ H.text "Type"
                                ]
                            ]
                        ]
                    , H.tbody []
                        []
                    ]
                ]
                :: accum

        CustomTypeDeclaration type_ ->
            H.div [ A.class "border rounded mb-3" ]
                [ H.table [ A.class "table table-striped table-borderless table-user-model mb-0" ]
                    [ H.caption [ A.style "caption-side" "top", A.class "section__title bpx-1 bpy-2" ] [ H.text ("type " ++ name) ]
                    , H.thead [ A.class "border-end border-start" ]
                        [ H.tr []
                            [ H.th [ A.class "w-33" ]
                                [ H.text "Constructor"
                                ]
                            , H.th []
                                [ H.text "Type"
                                ]
                            ]
                        ]
                    , H.tbody []
                        (List.map
                            (\node ->
                                let
                                    constructor =
                                        Node.value node
                                in
                                fieldView constructor.name (typeAnnotations constructor.arguments)
                            )
                            type_.constructors
                        )
                    ]
                ]
                :: accum

        _ ->
            accum



typeAnnotations : List (Node TypeAnnotation) -> String
typeAnnotations values =
    List.map (Node.value >> typeAnnotation) values
        |> String.join " "


typeAnnotation : TypeAnnotation -> String
typeAnnotation value =
    case value of
        GenericType name ->
            name

        Typed node annotations ->
            let
                ( moduleName, name ) =
                    Node.value node
            in
            String.join "." moduleName ++ name

        Unit ->
            "()"

        Tupled annotations ->
            let
                tuple =
                    List.map
                        (\node ->
                            typeAnnotation (Node.value node)
                        )
                        annotations
                        |> String.join ", "
            in
            "(" ++ tuple ++ ")"

        Record definition ->
            List.map
                (\node ->
                    let
                        ( name, annotation ) =
                            node
                                |> Node.value
                                |> Tuple.mapBoth Node.value Node.value
                    in
                    name ++ ":" ++ (typeAnnotation annotation)
                )
                definition
                |> String.join ", "

        GenericRecord name definition ->
            "{a | rec}"

        FunctionTypeAnnotation annotation1 annotation2 ->
            "->"


fieldView name type_ =
    H.tr []
        [ H.td []
            [ H.span
                (A.class "align-middle field bp-1"
                    :: DragDrop.draggable BindingDragDropMsg (Node.value name)
                )
                [ H.text (Node.value name) ]

            --, H.span [ A.class "text-success ml-1" ] [ Icons.link ]
            ]
        , H.td []
            [ H.span [ A.class "" ] [ H.text type_ ]
            ]
        ]



-- moduleName : Module -> String
-- moduleName module_ =
--     (case module_ of
--         NormalModule data ->
--             data.moduleName
--         PortModule data ->
--             data.moduleName
--         EffectModule data ->
--             data.moduleName
--     )
--         |> Node.value
--         |> String.join "."
-- userModelView : Model -> Html Msg
-- userModelView _ =
--     H.div [ A.class "bp-3 scroll-y", A.style "height" "350px", A.style "min-height" "350px" ]
--         [ H.div [ A.class "font-weight-500 mb-3" ]
--             [ H.text "Model" ]
--         , H.div [ A.class "border rounded mb-3" ]
--             [ H.table [ A.class "table table-striped table-borderless table-user-model mb-0" ]
--                 [ H.caption [ A.style "caption-side" "top", A.class "section__title  bpx-1 bpy-2" ] [ H.text "type alias Model" ]
--                 , H.thead [ A.class "border-bottom border-top" ]
--                     [ H.tr []
--                         [ H.th [ A.class "w-33" ]
--                             [ H.text "Field"
--                             ]
--                         , H.th []
--                             [ H.text "Type"
--                             ]
--                         ]
--                     ]
--                 , H.tbody []
--                     [ H.tr []
--                         [ H.td []
--                             [ H.span
--                                 (A.class "align-middle field bp-1"
--                                     :: DragDrop.draggable BindingDragDropMsg "users"
--                                 )
--                                 [ H.text "users" ]
--                             , H.span [ A.class "text-success ml-1" ] [ Icons.link ]
--                             ]
--                         , H.td []
--                             [ H.span [ A.class "" ] [ H.text "(List Int)" ]
--                             ]
--                         ]
--                     , H.tr []
--                         [ H.td []
--                             [ H.span [ A.class "field bp-1" ] [ H.text "counter" ]
--                             ]
--                         , H.td []
--                             [ H.span [ A.class "" ] [ H.text "Int" ]
--                             ]
--                         ]
--                     , H.tr []
--                         [ H.td []
--                             [ H.span [ A.class "field bp-1" ] [ H.text "status" ]
--                             ]
--                         , H.td []
--                             [ H.a [ A.href "#WebData", A.class "text-primary" ] [ H.text "WebData" ]
--                             ]
--                         ]
--                     , H.tr []
--                         [ H.td []
--                             [ H.span [ A.class "field bp-1" ] [ H.text "position" ]
--                             ]
--                         , H.td []
--                             [ H.a [ A.href "#Position", A.class "text-primary" ] [ H.text "Position" ]
--                             ]
--                         ]
--                     , H.tr []
--                         [ H.td []
--                             [ H.span [ A.class "field bp-1" ] [ H.text "users" ]
--                             ]
--                         , H.td []
--                             [ H.span [ A.class "" ] [ H.text "(Dict String User)" ]
--                             ]
--                         ]
--                     , H.tr []
--                         [ H.td []
--                             [ H.span [ A.class "field bp-1" ] [ H.text "fooBar" ]
--                             ]
--                         , H.td []
--                             [ H.span [ A.class "" ] [ H.text "String" ]
--                             ]
--                         ]
--                     ]
--                 ]
--             ]
--         , H.table [ A.id "WebData", A.class "table table-sm table-user-model" ]
--             [ H.caption [ A.style "caption-side" "top", A.class "section__title mb-2 bp-0" ] [ H.text "type WebData" ]
--             , H.thead []
--                 [ H.tr []
--                     [ H.th [ A.class "w-50" ]
--                         [ H.text "Constructor"
--                         ]
--                     , H.th []
--                         [ H.text "Type"
--                         ]
--                     ]
--                 ]
--             , H.tbody []
--                 [ H.tr []
--                     [ H.td []
--                         [ H.text "NotAsked"
--                         ]
--                     , H.td []
--                         [ H.span [ A.class "text-muted" ] [ H.text "(List Int)" ]
--                         ]
--                     ]
--                 , H.tr []
--                     [ H.td []
--                         [ H.text "Loading"
--                         ]
--                     , H.td []
--                         [ H.span [ A.class "text-muted" ] [ H.text "Int" ]
--                         ]
--                     ]
--                 , H.tr []
--                     [ H.td []
--                         [ H.text "Failure"
--                         ]
--                     , H.td []
--                         [ H.a [ A.href "#Error", A.class "text-primary" ] [ H.text "Error" ]
--                         ]
--                     ]
--                 , H.tr []
--                     [ H.td []
--                         [ H.text "Success"
--                         ]
--                     , H.td []
--                         [ H.span [ A.class "text-muted" ] [ H.text "(List " ]
--                         , H.a [ A.href "#User", A.class "text-primary" ] [ H.text "User" ]
--                         , H.span [ A.class "text-muted" ] [ H.text ")" ]
--                         ]
--                     ]
--                 ]
--             ]
--         , H.table [ A.id "User", A.class "table table-sm table-user-model" ]
--             [ H.caption [ A.style "caption-side" "top", A.class "section__title mb-2 bp-0" ] [ H.text "type alias User" ]
--             , H.thead []
--                 [ H.tr []
--                     [ H.th [ A.class "w-50" ]
--                         [ H.text "Field"
--                         ]
--                     , H.th []
--                         [ H.text "Type"
--                         ]
--                     ]
--                 ]
--             , H.tbody []
--                 [ H.tr []
--                     [ H.td []
--                         [ H.text "username"
--                         ]
--                     , H.td []
--                         [ H.span [ A.class "text-muted" ] [ H.text "String" ]
--                         ]
--                     ]
--                 , H.tr []
--                     [ H.td []
--                         [ H.text "avatarUrl"
--                         ]
--                     , H.td []
--                         [ H.span [ A.class "text-muted" ] [ H.text "String" ]
--                         ]
--                     ]
--                 , H.tr []
--                     [ H.td []
--                         [ H.text "enabled"
--                         ]
--                     , H.td []
--                         [ H.span [ A.class "text-muted" ] [ H.text "Bool" ]
--                         ]
--                     ]
--                 , H.tr []
--                     [ H.td []
--                         [ H.text "lastSeen"
--                         ]
--                     , H.td []
--                         [ H.span [ A.class "text-muted" ] [ H.text "Time.Posix" ]
--                         ]
--                     ]
--                 ]
--             ]
--         ]
