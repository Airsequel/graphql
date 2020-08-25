{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}

module Language.GraphQL.Type.Internal
    ( AbstractType(..)
    , CompositeType(..)
    , collectReferencedTypes
    , doesFragmentTypeApply
    , instanceOf
    , lookupTypeCondition
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL.AST (Name)
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema

-- | These types may describe the parent context of a selection set.
data CompositeType m
    = CompositeUnionType (Out.UnionType m)
    | CompositeObjectType (Out.ObjectType m)
    | CompositeInterfaceType (Out.InterfaceType m)
    deriving Eq

-- | These types may describe the parent context of a selection set.
data AbstractType m
    = AbstractUnionType (Out.UnionType m)
    | AbstractInterfaceType (Out.InterfaceType m)
    deriving Eq

-- | Traverses the schema and finds all referenced types.
collectReferencedTypes :: forall m. Schema m -> HashMap Name (Type m)
collectReferencedTypes schema =
    let queryTypes = traverseObjectType (query schema) HashMap.empty
     in maybe queryTypes (`traverseObjectType` queryTypes) $ mutation schema
  where
    collect traverser typeName element foundTypes
        | HashMap.member typeName foundTypes = foundTypes
        | otherwise = traverser $ HashMap.insert typeName element foundTypes
    visitFields (Out.Field _ outputType arguments) foundTypes
        = traverseOutputType outputType
        $ foldr visitArguments foundTypes arguments
    visitArguments (In.Argument _ inputType _) = traverseInputType inputType
    visitInputFields (In.InputField _ inputType _) = traverseInputType inputType
    getField (Out.ValueResolver field _) = field
    getField (Out.EventStreamResolver field _ _) = field
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
            traverser = polymorphicTraverser interfaces (getField <$> fields)
         in collect traverser typeName element foundTypes
    traverseInterfaceType interfaceType foundTypes =
        let (Out.InterfaceType typeName _ interfaces fields) = interfaceType
            element = InterfaceType interfaceType
            traverser = polymorphicTraverser interfaces fields
         in collect traverser typeName element foundTypes
    polymorphicTraverser interfaces fields
        = flip (foldr visitFields) fields
        . flip (foldr traverseInterfaceType) interfaces

doesFragmentTypeApply :: forall m
    . CompositeType m
    -> Out.ObjectType m
    -> Bool
doesFragmentTypeApply (CompositeObjectType fragmentType) objectType =
    fragmentType == objectType
doesFragmentTypeApply (CompositeInterfaceType fragmentType) objectType =
    instanceOf objectType $ AbstractInterfaceType fragmentType
doesFragmentTypeApply (CompositeUnionType fragmentType) objectType =
    instanceOf objectType $ AbstractUnionType fragmentType

instanceOf :: forall m. Out.ObjectType m -> AbstractType m -> Bool
instanceOf objectType (AbstractInterfaceType interfaceType) =
    let Out.ObjectType _ _ interfaces _ = objectType
     in foldr go False interfaces
  where
    go objectInterfaceType@(Out.InterfaceType _ _ interfaces _) acc =
        acc || foldr go (interfaceType == objectInterfaceType) interfaces
instanceOf objectType (AbstractUnionType unionType) =
    let Out.UnionType _ _ members = unionType
     in foldr go False members
  where
    go unionMemberType acc = acc || objectType == unionMemberType

lookupTypeCondition :: forall m
    . Name
    -> HashMap Name (Type m)
    -> Maybe (CompositeType m)
lookupTypeCondition type' types' =
    case HashMap.lookup type' types' of
        Just (ObjectType objectType) -> Just $ CompositeObjectType objectType
        Just (UnionType unionType) -> Just $ CompositeUnionType unionType
        Just (InterfaceType interfaceType) ->
            Just $ CompositeInterfaceType interfaceType
        _ -> Nothing
