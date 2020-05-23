{-# LANGUAGE ExplicitForAll #-}

-- | Schema Definition.
module Language.GraphQL.Type.Schema
    ( Schema(..)
    , collectReferencedTypes
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL.AST.Core (Name)
import Language.GraphQL.Type.Definition

-- | A Schema is created by supplying the root types of each type of operation,
--   query and mutation (optional). A schema definition is then supplied to the
--   validator and executor.
--
--   __Note:__ When the schema is constructed, by default only the types that
--   are reachable by traversing the root types are included, other types must
--   be explicitly referenced.
data Schema m = Schema
    { query :: ObjectType m
    , mutation :: Maybe (ObjectType m)
    }

-- | Traverses the schema and finds all referenced types.
collectReferencedTypes :: forall m. Schema m -> HashMap Name (TypeDefinition m)
collectReferencedTypes schema =
    let queryTypes = traverseObjectType (query schema) HashMap.empty
     in maybe queryTypes (`traverseObjectType` queryTypes) $ mutation schema
  where
    collect traverser typeName element foundTypes =
        let newMap = HashMap.insert typeName element foundTypes
         in maybe (traverser newMap) (const foundTypes)
            $ HashMap.lookup typeName foundTypes
    visitFields (Field _ outputType arguments _) foundTypes
        = traverseOutputType outputType
        $ foldr visitArguments foundTypes arguments
    visitArguments (Argument _ inputType _) = traverseInputType inputType
    visitInputFields (InputField _ inputType _) = traverseInputType inputType
    traverseInputType (ObjectInputTypeDefinition objectType) =
        let (InputObjectType typeName _ inputFields) = objectType
            element = InputObjectTypeDefinition objectType
            traverser = flip (foldr visitInputFields) inputFields
         in collect traverser typeName element
    traverseInputType (ListInputTypeDefinition listType) =
        traverseInputType listType
    traverseInputType (ScalarInputTypeDefinition scalarType) =
        let (ScalarType typeName _) = scalarType
         in collect Prelude.id typeName (ScalarTypeDefinition scalarType)
    traverseInputType (EnumInputTypeDefinition enumType) =
        let (EnumType typeName _ _) = enumType
         in collect Prelude.id typeName (EnumTypeDefinition enumType)
    traverseOutputType (ObjectOutputTypeDefinition objectType) =
        traverseObjectType objectType
    traverseOutputType (ListOutputTypeDefinition listType) =
        traverseOutputType listType
    traverseOutputType (ScalarOutputTypeDefinition scalarType) =
        let (ScalarType typeName _) = scalarType
         in collect Prelude.id typeName (ScalarTypeDefinition scalarType)
    traverseOutputType (EnumOutputTypeDefinition enumType) =
        let (EnumType typeName _ _) = enumType
         in collect Prelude.id typeName (EnumTypeDefinition enumType)
    traverseObjectType objectType foundTypes =
        let (ObjectType typeName _ objectFields) = objectType
            element = ObjectTypeDefinition objectType
            traverser = flip (foldr visitFields) objectFields
         in collect traverser typeName element foundTypes
