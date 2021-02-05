module Loader exposing
    ( declarations
    , parse
    )

{-| User model loader.
-}

import Elm.Parser
import Elm.Processing as Processing exposing (ProcessContext)
import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.File as File exposing (File)
import Elm.Syntax.Module as Module exposing (Module(..))
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Parser exposing (DeadEnd)


parse : String -> List String -> Result (List DeadEnd) File
parse main deps =
    let
        ctx =
            List.foldl
                (\source accum ->
                    case Elm.Parser.parse source of
                        Ok rawFile ->
                            Processing.addFile rawFile accum

                        Err _ ->
                            Debug.todo "Cannot parse dep."
                            --accum
                )
                Processing.init
                deps
    in
    Elm.Parser.parse main
        |> Result.map
            (\rawFile ->
                Processing.process ctx rawFile
            )


declarations : File -> List ( String, Node Declaration )
declarations file =
    let
        moduleName_ =
            file.moduleDefinition
                |> Node.value
                |> moduleName
    in
    List.map
        (\decl ->
            ( moduleName_ ++ "." ++ declarationName (Node.value decl), decl )
        )
        file.declarations


declarationName decl =
    case decl of
        FunctionDeclaration function ->
            -- TODO
            "function"

        AliasDeclaration alias_ ->
            alias_.name |> Node.value

        CustomTypeDeclaration type_ ->
            type_.name |> Node.value

        PortDeclaration signature ->
            signature.name |> Node.value

        InfixDeclaration infix ->
            -- TODO
            "infix"

        Destructuring _ _ ->
            "destructuring"


moduleName : Module -> String
moduleName module_ =
    (case module_ of
        NormalModule data ->
            data.moduleName

        PortModule data ->
            data.moduleName

        EffectModule data ->
            data.moduleName
    )
        |> Node.value
        |> String.join "."
