{-# LANGUAGE ExplicitForAll #-}

-- | Schema Definition.
module Language.GraphQL.Type.Schema
    ( CompositeType(..)
    , Schema(..)
    , Type(..)
    , collectReferencedTypes
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL.AST.Document (Name)
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out

-- | These are all of the possible kinds of types.
data Type m
    = ScalarType Definition.ScalarType
    | EnumType Definition.EnumType
    | ObjectType (Out.ObjectType m)
    | InputObjectType In.InputObjectType
    | InterfaceType (Out.InterfaceType m)
    | UnionType (Out.UnionType m)

-- | These types may describe the parent context of a selection set.
data CompositeType m
    = CompositeUnionType (Out.UnionType m)
    | CompositeObjectType (Out.ObjectType m)
    | CompositeInterfaceType (Out.InterfaceType m)

-- | A Schema is created by supplying the root types of each type of operation,
--   query and mutation (optional). A schema definition is then supplied to the
--   validator and executor.
--
--   __Note:__ When the schema is constructed, by default only the types that
--   are reachable by traversing the root types are included, other types must
--   be explicitly referenced.
data Schema m = Schema
    { query :: Out.ObjectType m
    , mutation :: Maybe (Out.ObjectType m)
    }

-- | Traverses the schema and finds all referenced types.
collectReferencedTypes :: forall m. Schema m -> HashMap Name (Type m)
collectReferencedTypes schema =
    let queryTypes = traverseObjectType (query schema) HashMap.empty
     in maybe queryTypes (`traverseObjectType` queryTypes) $ mutation schema
  where
    collect traverser typeName element foundTypes
        | HashMap.member typeName foundTypes = foundTypes
        | otherwise = traverser $ HashMap.insert typeName element foundTypes
    visitFields (Out.Field _ outputType arguments _) foundTypes
        = traverseOutputType outputType
        $ foldr visitArguments foundTypes arguments
    visitArguments (In.Argument _ inputType _) = traverseInputType inputType
    visitInputFields (In.InputField _ inputType _) = traverseInputType inputType
    traverseInputType (In.InputObjectBaseType objectType) =
        let (In.InputObjectType typeName _ inputFields) = objectType
            element = InputObjectType objectType
            traverser = flip (foldr visitInputFields) inputFields
         in collect traverser typeName element
    traverseInputType (In.ListBaseType listType) =
        traverseInputType listType
    traverseInputType (In.ScalarBaseType scalarType) =
        let (Definition.ScalarType typeName _) = scalarType
         in collect Prelude.id typeName (ScalarType scalarType)
    traverseInputType (In.EnumBaseType enumType) =
        let (Definition.EnumType typeName _ _) = enumType
         in collect Prelude.id typeName (EnumType enumType)
    traverseOutputType (Out.ObjectBaseType objectType) =
        traverseObjectType objectType
    traverseOutputType (Out.InterfaceBaseType interfaceType) =
        traverseInterfaceType interfaceType
    traverseOutputType (Out.UnionBaseType unionType) =
        let (Out.UnionType typeName _ types) = unionType
            traverser = flip (foldr traverseObjectType) types
         in collect traverser typeName (UnionType unionType)
    traverseOutputType (Out.ListBaseType listType) =
        traverseOutputType listType
    traverseOutputType (Out.ScalarBaseType scalarType) =
        let (Definition.ScalarType typeName _) = scalarType
         in collect Prelude.id typeName (ScalarType scalarType)
    traverseOutputType (Out.EnumBaseType enumType) =
        let (Definition.EnumType typeName _ _) = enumType
         in collect Prelude.id typeName (EnumType enumType)
    traverseObjectType objectType foundTypes =
        let (Out.ObjectType typeName _ interfaces fields) = objectType
            element = ObjectType objectType
            traverser = polymorphicTypeTraverser interfaces fields
         in collect traverser typeName element foundTypes
    traverseInterfaceType interfaceType foundTypes =
        let (Out.InterfaceType typeName _ interfaces fields) = interfaceType
            element = InterfaceType interfaceType
            traverser = polymorphicTypeTraverser interfaces fields
         in collect traverser typeName element foundTypes
    polymorphicTypeTraverser interfaces fields
        = flip (foldr visitFields) fields
        . flip (foldr traverseInterfaceType) interfaces
