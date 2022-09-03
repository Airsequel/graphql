{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a representation of a @GraphQL@ Schema in addition to
-- functions for defining and manipulating schemas.
module Language.GraphQL.Type.Schema
    ( schema
    , schemaWithTypes
    , module Language.GraphQL.Type.Internal
    ) where

import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import Language.GraphQL.AST.DirectiveLocation (DirectiveLocation(..))
import qualified Language.GraphQL.AST.DirectiveLocation as DirectiveLocation
import qualified Language.GraphQL.AST as Full
import Language.GraphQL.Type.Internal
    ( Directive(..)
    , Directives
    , Schema
    , Type(..)
    , description
    , directives
    , implementations
    , mutation
    , subscription
    , query
    , types
    )
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.Internal as Internal
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out

-- | Schema constructor.
--
-- __Note:__ When the schema is constructed, by default only the types that
-- are reachable by traversing the root types are included, other types must
-- be explicitly referenced using 'schemaWithTypes' instead.
schema :: forall m
    . Out.ObjectType m -- ^ Query type.
    -> Maybe (Out.ObjectType m) -- ^ Mutation type.
    -> Maybe (Out.ObjectType m) -- ^ Subscription type.
    -> Directives -- ^ Directive definitions.
    -> Schema m -- ^ Schema.
schema queryRoot mutationRoot subscriptionRoot =
    schemaWithTypes Nothing queryRoot mutationRoot subscriptionRoot mempty

-- | Constructs a complete schema, including user-defined types not referenced
-- in the schema directly (for example interface implementations).
schemaWithTypes :: forall m
    . Maybe Text -- ^ Schema description
    -> Out.ObjectType m -- ^ Query type.
    -> Maybe (Out.ObjectType m) -- ^ Mutation type.
    -> Maybe (Out.ObjectType m) -- ^ Subscription type.
    -> [Type m] -- ^ Additional types.
    -> Directives -- ^ Directive definitions.
    -> Schema m -- ^ Schema.
schemaWithTypes description' queryRoot mutationRoot subscriptionRoot types' directiveDefinitions =
    Internal.Schema description' queryRoot mutationRoot subscriptionRoot
        allDirectives collectedTypes collectedImplementations
  where
    allTypes = foldr addTypeDefinition KeyMap.empty types'
    addTypeDefinition type'@(ScalarType (Definition.ScalarType typeName _)) accumulator =
        KeyMap.insert typeName type' accumulator
    addTypeDefinition type'@(EnumType (Definition.EnumType typeName _ _)) accumulator =
        KeyMap.insert typeName type' accumulator
    addTypeDefinition type'@(ObjectType (Out.ObjectType typeName _ _ _)) accumulator =
        KeyMap.insert (Key.fromText typeName) type' accumulator
    addTypeDefinition type'@(InputObjectType (In.InputObjectType typeName _ _)) accumulator =
        KeyMap.insert typeName type' accumulator
    addTypeDefinition type'@(InterfaceType (Out.InterfaceType typeName _ _ _)) accumulator =
        KeyMap.insert (Key.fromText typeName) type' accumulator
    addTypeDefinition type'@(UnionType (Out.UnionType typeName _ _)) accumulator =
        KeyMap.insert (Key.fromText typeName) type' accumulator
    collectedTypes = collectReferencedTypes queryRoot mutationRoot subscriptionRoot allTypes
    collectedImplementations = collectImplementations collectedTypes
    allDirectives = KeyMap.union directiveDefinitions defaultDirectives
    defaultDirectives = KeyMap.fromList
        [ ("skip", skipDirective)
        , ("include", includeDirective)
        , ("deprecated", deprecatedDirective)
        ]
    includeDirective =
        Directive includeDescription skipIncludeLocations includeArguments
    includeArguments = KeyMap.singleton "if"
        $ In.Argument (Just "Included when true.") ifType Nothing
    includeDescription = Just
        "Directs the executor to include this field or fragment only when the \
        \`if` argument is true."
    skipDirective = Directive skipDescription skipIncludeLocations skipArguments
    skipArguments = KeyMap.singleton "if"
        $ In.Argument (Just "skipped when true.") ifType Nothing
    ifType = In.NonNullScalarType Definition.boolean
    skipDescription = Just
        "Directs the executor to skip this field or fragment when the `if` \
        \argument is true."
    skipIncludeLocations =
        [ ExecutableDirectiveLocation DirectiveLocation.Field
        , ExecutableDirectiveLocation DirectiveLocation.FragmentSpread
        , ExecutableDirectiveLocation DirectiveLocation.InlineFragment
        ]
    deprecatedDirective =
        Directive deprecatedDescription deprecatedLocations deprecatedArguments
    reasonDescription = Just
        "Explains why this element was deprecated, usually also including a \
        \suggestion for how to access supported similar data. Formatted using \
        \the Markdown syntax, as specified by \
        \[CommonMark](https://commonmark.org/).'"
    deprecatedArguments = KeyMap.singleton "reason"
        $ In.Argument reasonDescription reasonType
        $ Just "No longer supported"
    reasonType = In.NamedScalarType Definition.string
    deprecatedDescription = Just
        "Marks an element of a GraphQL schema as no longer supported."
    deprecatedLocations =
        [ TypeSystemDirectiveLocation DirectiveLocation.FieldDefinition
        , TypeSystemDirectiveLocation DirectiveLocation.ArgumentDefinition
        , TypeSystemDirectiveLocation DirectiveLocation.InputFieldDefinition
        , TypeSystemDirectiveLocation DirectiveLocation.EnumValue
        ]

-- | Traverses the schema and finds all referenced types.
collectReferencedTypes :: forall m
    . Out.ObjectType m
    -> Maybe (Out.ObjectType m)
    -> Maybe (Out.ObjectType m)
    -> KeyMap (Type m)
    -> KeyMap (Type m)
collectReferencedTypes queryRoot mutationRoot subscriptionRoot extraTypes =
    let queryTypes = traverseObjectType queryRoot extraTypes
        mutationTypes = maybe queryTypes (`traverseObjectType` queryTypes)
            mutationRoot
     in maybe mutationTypes (`traverseObjectType` mutationTypes) subscriptionRoot
  where
    collect traverser typeName element foundTypes
        | KeyMap.member typeName foundTypes = foundTypes
        | otherwise = traverser $ KeyMap.insert typeName element foundTypes
    visitFields (Out.Field _ outputType arguments) foundTypes
        = traverseOutputType outputType
        $ foldr visitArguments foundTypes arguments
    visitArguments (In.Argument _ inputType _) = traverseInputType inputType
    visitInputFields (In.InputField _ inputType _) = traverseInputType inputType
    getField (Out.ValueResolver field _) = field
    getField (Out.EventStreamResolver field _ _) = field
    traverseInputType (In.InputObjectBaseType objectType) =
        let In.InputObjectType typeName _ inputFields = objectType
            element = InputObjectType objectType
            traverser = flip (foldr visitInputFields) inputFields
         in collect traverser typeName element
    traverseInputType (In.ListBaseType listType) =
        traverseInputType listType
    traverseInputType (In.ScalarBaseType scalarType) =
        let Definition.ScalarType typeName _ = scalarType
         in collect Prelude.id typeName (ScalarType scalarType)
    traverseInputType (In.EnumBaseType enumType) =
        let Definition.EnumType typeName _ _ = enumType
         in collect Prelude.id typeName (EnumType enumType)
    traverseOutputType (Out.ObjectBaseType objectType) =
        traverseObjectType objectType
    traverseOutputType (Out.InterfaceBaseType interfaceType) =
        traverseInterfaceType interfaceType
    traverseOutputType (Out.UnionBaseType unionType) =
        let Out.UnionType typeName _ types' = unionType
            traverser = flip (foldr traverseObjectType) types'
         in collect traverser (Key.fromText typeName) (UnionType unionType)
    traverseOutputType (Out.ListBaseType listType) =
        traverseOutputType listType
    traverseOutputType (Out.ScalarBaseType scalarType) =
        let Definition.ScalarType typeName _ = scalarType
         in collect Prelude.id typeName (ScalarType scalarType)
    traverseOutputType (Out.EnumBaseType enumType) =
        let Definition.EnumType typeName _ _ = enumType
         in collect Prelude.id typeName (EnumType enumType)
    traverseObjectType objectType foundTypes =
        let Out.ObjectType typeName _ interfaces fields = objectType
            element = ObjectType objectType
            traverser = polymorphicTraverser interfaces (getField <$> fields)
         in collect traverser (Key.fromText typeName) element foundTypes
    traverseInterfaceType interfaceType foundTypes =
        let Out.InterfaceType typeName _ interfaces fields = interfaceType
            element = InterfaceType interfaceType
            traverser = polymorphicTraverser interfaces fields
         in collect traverser (Key.fromText typeName) element foundTypes
    polymorphicTraverser interfaces fields
        = flip (foldr visitFields) fields
        . flip (foldr traverseInterfaceType) interfaces

-- | Looks for objects and interfaces under the schema types and collects the
-- interfaces they implement.
collectImplementations :: forall m
    . KeyMap (Type m)
    -> KeyMap [Type m]
collectImplementations = KeyMap.foldr go KeyMap.empty
  where
    go implementation@(InterfaceType interfaceType) accumulator =
        let Out.InterfaceType _ _ interfaces _ = interfaceType
         in foldr (add implementation) accumulator interfaces
    go implementation@(ObjectType objectType) accumulator =
        let Out.ObjectType _ _ interfaces _ = objectType
         in foldr (add implementation) accumulator interfaces
    go _ accumulator = accumulator
    add implementation (Out.InterfaceType typeName _ _ _) =
        -- KeyMap.insertWith (++) typeName [implementation]
        KeyMap.insert (Key.fromText typeName) [implementation]

