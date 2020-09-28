{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}

module Language.GraphQL.Type.Internal
    ( AbstractType(..)
    , CompositeType(..)
    , collectReferencedTypes
    , doesFragmentTypeApply
    , instanceOf
    , lookupInputType
    , lookupTypeCondition
    , lookupTypeField
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Language.GraphQL.AST as Full
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema (Schema)
import qualified Language.GraphQL.Type.Schema as Schema

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
collectReferencedTypes :: forall m
    . Schema m
    -> HashMap Full.Name (Schema.Type m)
collectReferencedTypes schema =
    let queryTypes = traverseObjectType (Schema.query schema) HashMap.empty
        mutationTypes = maybe queryTypes (`traverseObjectType` queryTypes)
            $ Schema.mutation schema
     in maybe mutationTypes (`traverseObjectType` queryTypes)
        $ Schema.subscription schema
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
        let In.InputObjectType typeName _ inputFields = objectType
            element = Schema.InputObjectType objectType
            traverser = flip (foldr visitInputFields) inputFields
         in collect traverser typeName element
    traverseInputType (In.ListBaseType listType) =
        traverseInputType listType
    traverseInputType (In.ScalarBaseType scalarType) =
        let Definition.ScalarType typeName _ = scalarType
         in collect Prelude.id typeName (Schema.ScalarType scalarType)
    traverseInputType (In.EnumBaseType enumType) =
        let Definition.EnumType typeName _ _ = enumType
         in collect Prelude.id typeName (Schema.EnumType enumType)
    traverseOutputType (Out.ObjectBaseType objectType) =
        traverseObjectType objectType
    traverseOutputType (Out.InterfaceBaseType interfaceType) =
        traverseInterfaceType interfaceType
    traverseOutputType (Out.UnionBaseType unionType) =
        let Out.UnionType typeName _ types = unionType
            traverser = flip (foldr traverseObjectType) types
         in collect traverser typeName (Schema.UnionType unionType)
    traverseOutputType (Out.ListBaseType listType) =
        traverseOutputType listType
    traverseOutputType (Out.ScalarBaseType scalarType) =
        let Definition.ScalarType typeName _ = scalarType
         in collect Prelude.id typeName (Schema.ScalarType scalarType)
    traverseOutputType (Out.EnumBaseType enumType) =
        let Definition.EnumType typeName _ _ = enumType
         in collect Prelude.id typeName (Schema.EnumType enumType)
    traverseObjectType objectType foundTypes =
        let Out.ObjectType typeName _ interfaces fields = objectType
            element = Schema.ObjectType objectType
            traverser = polymorphicTraverser interfaces (getField <$> fields)
         in collect traverser typeName element foundTypes
    traverseInterfaceType interfaceType foundTypes =
        let Out.InterfaceType typeName _ interfaces fields = interfaceType
            element = Schema.InterfaceType interfaceType
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
    . Full.Name
    -> HashMap Full.Name (Schema.Type m)
    -> Maybe (CompositeType m)
lookupTypeCondition type' types' =
    case HashMap.lookup type' types' of
        Just (Schema.ObjectType objectType) ->
            Just $ CompositeObjectType objectType
        Just (Schema.UnionType unionType) -> Just $ CompositeUnionType unionType
        Just (Schema.InterfaceType interfaceType) ->
            Just $ CompositeInterfaceType interfaceType
        _ -> Nothing

lookupInputType
    :: Full.Type
    -> HashMap.HashMap Full.Name (Schema.Type m)
    -> Maybe In.Type
lookupInputType (Full.TypeNamed name) types =
    case HashMap.lookup name types of
        Just (Schema.ScalarType scalarType) ->
            Just $ In.NamedScalarType scalarType
        Just (Schema.EnumType enumType) ->
            Just $ In.NamedEnumType enumType
        Just (Schema.InputObjectType objectType) ->
            Just $ In.NamedInputObjectType objectType
        _ -> Nothing
lookupInputType (Full.TypeList list) types
    = In.ListType
    <$> lookupInputType list types
lookupInputType (Full.TypeNonNull (Full.NonNullTypeNamed nonNull)) types  =
    case HashMap.lookup nonNull types of
        Just (Schema.ScalarType scalarType) ->
            Just $ In.NonNullScalarType scalarType
        Just (Schema.EnumType enumType) ->
            Just $ In.NonNullEnumType enumType
        Just (Schema.InputObjectType objectType) ->
            Just $ In.NonNullInputObjectType objectType
        _ -> Nothing
lookupInputType (Full.TypeNonNull (Full.NonNullTypeList nonNull)) types
    = In.NonNullListType
    <$> lookupInputType nonNull types

lookupTypeField :: forall a. Full.Name -> Out.Type a -> Maybe (Out.Field a)
lookupTypeField fieldName = \case
    Out.ObjectBaseType objectType ->
        objectChild objectType
    Out.InterfaceBaseType interfaceType ->
        interfaceChild interfaceType
    Out.ListBaseType listType -> lookupTypeField fieldName listType
    _ -> Nothing
  where
    objectChild (Out.ObjectType _ _ _ resolvers) =
        resolverType <$> HashMap.lookup fieldName resolvers
    interfaceChild (Out.InterfaceType _ _ _ fields) =
        HashMap.lookup fieldName fields
    resolverType (Out.ValueResolver objectField _) = objectField
    resolverType (Out.EventStreamResolver objectField _ _) = objectField
