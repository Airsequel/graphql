{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | GraphQL validator.
module Language.GraphQL.Validate
    ( Validation.Error(..)
    , document
    , module Language.GraphQL.Validate.Rules
    ) where

import Control.Monad (join)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq(..), (><), (|>))
import qualified Data.Sequence as Seq
import Language.GraphQL.AST.DirectiveLocation (DirectiveLocation(..))
import qualified Language.GraphQL.AST.DirectiveLocation as DirectiveLocation
import qualified Language.GraphQL.AST.Document as Full
import Language.GraphQL.Type.Internal
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema (Schema)
import qualified Language.GraphQL.Type.Schema as Schema
import Language.GraphQL.Validate.Rules
import Language.GraphQL.Validate.Validation (Validation(Validation))
import qualified Language.GraphQL.Validate.Validation as Validation

type ApplySelectionRule m a
    = HashMap Full.Name (Schema.Type m)
    -> Validation.Rule m
    -> Maybe (Out.Type m)
    -> a
    -> Seq (Validation.RuleT m)

type ApplyRule m a = Validation.Rule m -> a -> Seq (Validation.RuleT m)

-- | Validates a document and returns a list of found errors. If the returned
-- list is empty, the document is valid.
document :: forall m
    . Schema m
    -> [Validation.Rule m]
    -> Full.Document
    -> Seq Validation.Error
document schema' rules' document' =
    runReaderT reader context
  where
    context = Validation
        { Validation.ast = document'
        , Validation.schema = schema'
        , Validation.types = collectReferencedTypes schema'
        , Validation.directives = allDirectives
        }
    allDirectives = 
        HashMap.union (Schema.directives schema') defaultDirectives
    defaultDirectives = HashMap.fromList
        [ ("skip", skipDirective)
        , ("include", includeDirective)
        , ("deprecated", deprecatedDirective)
        ]
    includeDirective =
        Schema.Directive includeDescription skipIncludeLocations includeArguments
    includeArguments = HashMap.singleton "if"
        $ In.Argument (Just "Included when true.") ifType Nothing
    includeDescription = Just
        "Directs the executor to include this field or fragment only when the \
        \`if` argument is true."
    skipDirective =
        Schema.Directive skipDescription skipIncludeLocations skipArguments
    skipArguments = HashMap.singleton "if"
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
        Schema.Directive deprecatedDescription deprecatedLocations deprecatedArguments
    reasonDescription = Just
        "Explains why this element was deprecated, usually also including a \
        \suggestion for how to access supported similar data. Formatted using \
        \the Markdown syntax, as specified by \
        \[CommonMark](https://commonmark.org/).'"
    deprecatedArguments = HashMap.singleton "reason"
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
    reader = do
        rule' <- lift $ Seq.fromList rules'
        join $ lift $ foldr (definition rule' context) Seq.empty document'

definition :: Validation.Rule m
    -> Validation m
    -> Full.Definition
    -> Seq (Validation.RuleT m)
    -> Seq (Validation.RuleT m)
definition (Validation.DefinitionRule rule) _ definition' accumulator =
    accumulator |> rule definition'
definition rule context (Full.ExecutableDefinition definition') accumulator =
    accumulator >< executableDefinition rule context definition'
definition rule context (Full.TypeSystemDefinition typeSystemDefinition' _) accumulator =
    accumulator >< typeSystemDefinition context rule typeSystemDefinition'
definition rule context (Full.TypeSystemExtension extension _) accumulator =
    accumulator >< typeSystemExtension context rule extension

typeSystemExtension :: forall m
    . Validation m
    -> ApplyRule m Full.TypeSystemExtension
typeSystemExtension context rule = \case
    Full.SchemaExtension extension -> schemaExtension context rule extension
    Full.TypeExtension extension -> typeExtension context rule extension

typeExtension :: forall m. Validation m -> ApplyRule m Full.TypeExtension
typeExtension context rule = \case
    Full.ScalarTypeExtension _ directives' -> directives context rule directives'
    Full.ObjectTypeFieldsDefinitionExtension _ _ directives' fields
        -> directives context rule directives'
        >< foldMap (fieldDefinition context rule) fields
    Full.ObjectTypeDirectivesExtension _ _ directives' ->
        directives context rule directives'
    Full.ObjectTypeImplementsInterfacesExtension _ _ -> mempty
    Full.InterfaceTypeFieldsDefinitionExtension _ directives' fields
        -> directives context rule directives'
        >< foldMap (fieldDefinition context rule) fields
    Full.InterfaceTypeDirectivesExtension _ directives' ->
        directives context rule directives'
    Full.UnionTypeUnionMemberTypesExtension _ directives' _ ->
        directives context rule directives'
    Full.UnionTypeDirectivesExtension _ directives' ->
        directives context rule directives'
    Full.EnumTypeEnumValuesDefinitionExtension _ directives' values
        -> directives context rule directives'
        >< foldMap (enumValueDefinition context rule) values
    Full.EnumTypeDirectivesExtension _ directives' ->
        directives context rule directives'
    Full.InputObjectTypeInputFieldsDefinitionExtension _ directives' fields
        -> directives context rule directives'
        >< foldMap (inputValueDefinition context rule) fields
    Full.InputObjectTypeDirectivesExtension _ directives' ->
        directives context rule directives'

schemaExtension :: forall m. Validation m -> ApplyRule m Full.SchemaExtension
schemaExtension context rule = \case
    Full.SchemaOperationExtension directives' _ ->
        directives context rule directives'
    Full.SchemaDirectivesExtension directives' -> directives context rule directives'

executableDefinition :: forall m
    . Validation.Rule m
    -> Validation m
    -> Full.ExecutableDefinition
    -> Seq (Validation.RuleT m)
executableDefinition rule context (Full.DefinitionOperation operation) =
    operationDefinition rule context operation
executableDefinition rule context (Full.DefinitionFragment fragment) =
    fragmentDefinition rule context fragment

typeSystemDefinition :: forall m
    . Validation m
    -> ApplyRule m Full.TypeSystemDefinition
typeSystemDefinition context rule = \case
    Full.SchemaDefinition directives' _ -> directives context rule directives'
    Full.TypeDefinition typeDefinition' ->
        typeDefinition context rule typeDefinition'
    Full.DirectiveDefinition _ _ arguments' _ ->
        argumentsDefinition context rule arguments'

typeDefinition :: forall m. Validation m -> ApplyRule m Full.TypeDefinition
typeDefinition context rule = \case
    Full.ScalarTypeDefinition _ _ directives' ->
        directives context rule directives'
    Full.ObjectTypeDefinition _ _ _ directives' fields
        -> directives context rule directives'
         >< foldMap (fieldDefinition context rule) fields
    Full.InterfaceTypeDefinition _ _ directives' fields
        -> directives context rule directives'
        >< foldMap (fieldDefinition context rule) fields
    Full.UnionTypeDefinition _ _ directives' _ ->
        directives context rule directives'
    Full.EnumTypeDefinition _ _ directives' values
        -> directives context rule directives'
        >< foldMap (enumValueDefinition context rule) values
    Full.InputObjectTypeDefinition _ _ directives' fields
        -> directives context rule directives'
        <> foldMap (inputValueDefinition context rule) fields

enumValueDefinition :: forall m
    . Validation m
    -> ApplyRule m Full.EnumValueDefinition
enumValueDefinition context rule (Full.EnumValueDefinition _ _ directives') =
    directives context rule directives'

fieldDefinition :: forall m. Validation m -> ApplyRule m Full.FieldDefinition
fieldDefinition context rule (Full.FieldDefinition _ _ arguments' _ directives')
    = directives context rule directives'
    >< argumentsDefinition context rule arguments'

argumentsDefinition :: forall m
    . Validation m
    -> ApplyRule m Full.ArgumentsDefinition
argumentsDefinition context rule (Full.ArgumentsDefinition definitions) =
    foldMap (inputValueDefinition context rule) definitions

inputValueDefinition :: forall m
    . Validation m
    -> ApplyRule m Full.InputValueDefinition
inputValueDefinition context rule (Full.InputValueDefinition _ _ _ _ directives') =
    directives context rule directives'

operationDefinition :: forall m
    . Validation.Rule m
    -> Validation m
    -> Full.OperationDefinition
    -> Seq (Validation.RuleT m)
operationDefinition rule context operation
    | Validation.OperationDefinitionRule operationRule <- rule =
        pure $ operationRule operation
    | Validation.VariablesRule variablesRule <- rule
    , Full.OperationDefinition _ _ variables _ _ _ <- operation =
        foldMap (variableDefinition context rule) variables |> variablesRule variables
    | Full.SelectionSet selections _ <- operation =
        selectionSet context types' rule (getRootType Full.Query) selections
    | Full.OperationDefinition operationType _ _ directives' selections _  <- operation
        = selectionSet context types' rule (getRootType operationType) selections
        >< directives context rule directives'
  where
    types' = Validation.types context
    getRootType Full.Query =
       Just $ Out.NamedObjectType $ Schema.query $ Validation.schema context
    getRootType Full.Mutation =
       Out.NamedObjectType <$> Schema.mutation (Validation.schema context)
    getRootType Full.Subscription =
        Out.NamedObjectType <$> Schema.subscription (Validation.schema context)
        
typeToOut :: forall m. Schema.Type m -> Maybe (Out.Type m)
typeToOut (Schema.ObjectType objectType) =
    Just $ Out.NamedObjectType objectType
typeToOut (Schema.InterfaceType interfaceType) =
    Just $ Out.NamedInterfaceType interfaceType
typeToOut (Schema.UnionType unionType) = Just $ Out.NamedUnionType unionType
typeToOut (Schema.EnumType enumType) = Just $ Out.NamedEnumType enumType
typeToOut (Schema.ScalarType scalarType) = Just $ Out.NamedScalarType scalarType
typeToOut _ = Nothing

variableDefinition :: forall m
    . Validation m
    -> ApplyRule m Full.VariableDefinition
variableDefinition context rule (Full.VariableDefinition _ typeName value' _)
    | Just defaultValue' <- value'
    , variableType <- lookupInputType typeName $ Validation.types context =
        constValue rule variableType $ Full.value defaultValue'
variableDefinition _ _ _ = mempty

constValue :: forall m
    . Validation.Rule m
    -> Maybe In.Type
    -> Full.ConstValue
    -> Seq (Validation.RuleT m)
constValue (Validation.ValueRule _ rule) valueType = go valueType
  where
    go inputObjectType value'@(Full.ConstObject fields)
        = foldMap (forEach inputObjectType) (Seq.fromList fields)
        |> rule inputObjectType value'
    go listType value'@(Full.ConstList values)
        = foldMap (go $ valueTypeFromList listType) (Seq.fromList values)
        |> rule listType value'
    go anotherValue value' = pure $ rule anotherValue value'
    forEach inputObjectType (Full.ObjectField fieldName fieldValue _) =
        go (valueTypeByName fieldName inputObjectType) fieldValue
constValue _ _ = const mempty

inputFieldType :: In.InputField -> In.Type
inputFieldType (In.InputField _ inputFieldType' _) = inputFieldType'

valueTypeByName :: Full.Name -> Maybe In.Type -> Maybe In.Type
valueTypeByName fieldName (Just( In.InputObjectBaseType inputObjectType)) =
    let In.InputObjectType _ _ fieldTypes = inputObjectType
     in inputFieldType <$> HashMap.lookup fieldName fieldTypes
valueTypeByName _ _ = Nothing

valueTypeFromList :: Maybe In.Type -> Maybe In.Type
valueTypeFromList (Just (In.ListBaseType listType)) = Just listType
valueTypeFromList _ = Nothing

fragmentDefinition :: forall m
    . Validation.Rule m
    -> Validation m
    -> Full.FragmentDefinition
    -> Seq (Validation.RuleT m)
fragmentDefinition (Validation.FragmentDefinitionRule rule) _ definition' =
    pure $ rule definition'
fragmentDefinition rule context definition'
    | Full.FragmentDefinition _ typeCondition directives' selections _ <- definition'
    , Validation.FragmentRule definitionRule _ <- rule
        = applyToChildren typeCondition directives' selections
        |> definitionRule definition'
    | Full.FragmentDefinition _ typeCondition directives' selections _ <- definition'
        = applyToChildren typeCondition directives' selections
  where
    types' = Validation.types context
    applyToChildren typeCondition directives' selections
        = selectionSet context types' rule (lookupType' typeCondition) selections
        >< directives context rule directives'
    lookupType' = flip lookupType types'

lookupType :: forall m
    . Full.TypeCondition
    -> HashMap Full.Name (Schema.Type m)
    -> Maybe (Out.Type m)
lookupType typeCondition types' = HashMap.lookup typeCondition types'
    >>= typeToOut

selectionSet :: Traversable t
    => forall m
    . Validation m
    -> ApplySelectionRule m (t Full.Selection)
selectionSet context types' rule = foldMap . selection context types' rule

selection :: forall m. Validation m -> ApplySelectionRule m Full.Selection
selection context types' rule objectType selection'
    | Validation.SelectionRule selectionRule <- rule =
        applyToChildren |> selectionRule objectType selection'
    | otherwise = applyToChildren
  where
    applyToChildren =
        case selection' of
            Full.FieldSelection field' ->
                field context types' rule objectType field'
            Full.InlineFragmentSelection inlineFragment' ->
                inlineFragment context types' rule objectType inlineFragment'
            Full.FragmentSpreadSelection fragmentSpread' ->
                fragmentSpread context rule fragmentSpread'

field :: forall m. Validation m -> ApplySelectionRule m Full.Field
field context types' rule objectType field' = go field'
  where
    go (Full.Field _ fieldName _ _ _ _)
        | Validation.FieldRule fieldRule <- rule =
            applyToChildren fieldName |> fieldRule objectType field'
        | Validation.ArgumentsRule argumentsRule _  <- rule =
            applyToChildren fieldName |> argumentsRule objectType field'
        | otherwise = applyToChildren fieldName
    typeFieldType (Out.Field _ type' _) = type'
    typeFieldArguments (Out.Field _ _ argumentTypes) = argumentTypes
    applyToChildren fieldName =
        let Full.Field _ _ arguments' directives' selections _ = field'
            typeField = objectType >>= lookupTypeField fieldName
            argumentTypes = maybe mempty typeFieldArguments typeField
         in selectionSet context types' rule (typeFieldType <$> typeField) selections
            >< directives context rule directives'
            >< arguments rule argumentTypes arguments'

arguments :: forall m
    . Validation.Rule m
    -> In.Arguments
    -> [Full.Argument]
    -> Seq (Validation.RuleT m)
arguments rule argumentTypes = foldMap forEach . Seq.fromList
  where
    forEach argument'@(Full.Argument argumentName _ _) = 
       let argumentType = HashMap.lookup argumentName argumentTypes
        in argument rule argumentType argument'

argument :: forall m
    . Validation.Rule m
    -> Maybe In.Argument
    -> Full.Argument
    -> Seq (Validation.RuleT m)
argument rule argumentType (Full.Argument _ value' _) =
    value rule (valueType <$> argumentType) $ Full.value value'
  where
    valueType (In.Argument _ valueType' _) = valueType'

value :: forall m
    . Validation.Rule m
    -> Maybe In.Type
    -> Full.Value
    -> Seq (Validation.RuleT m)
value (Validation.ValueRule rule _) valueType = go valueType
  where
    go inputObjectType value'@(Full.Object fields)
        = foldMap (forEach inputObjectType) (Seq.fromList fields)
        |> rule inputObjectType value'
    go listType value'@(Full.List values)
        = foldMap (go $ valueTypeFromList listType) (Seq.fromList values)
        |> rule listType value'
    go anotherValue value' = pure $ rule anotherValue value'
    forEach inputObjectType (Full.ObjectField fieldName fieldValue _) =
        go (valueTypeByName fieldName inputObjectType) fieldValue
value _ _ = const mempty

inlineFragment :: forall m
    . Validation m
    -> ApplySelectionRule m Full.InlineFragment
inlineFragment context types' rule objectType inlineFragment' =
    go inlineFragment'
  where
    go (Full.InlineFragment optionalType directives' selections _)
        | Validation.FragmentRule _ fragmentRule <- rule
            = applyToChildren (refineTarget optionalType) directives' selections
            |> fragmentRule inlineFragment'
        | otherwise = applyToChildren (refineTarget optionalType) directives' selections
    refineTarget (Just typeCondition) = lookupType typeCondition types'
    refineTarget Nothing = objectType
    applyToChildren objectType' directives' selections
        = selectionSet context types' rule objectType' selections
        >< directives context rule directives'

fragmentSpread :: forall m. Validation m -> ApplyRule m Full.FragmentSpread
fragmentSpread context rule fragmentSpread'@(Full.FragmentSpread _ directives' _)
    | Validation.FragmentSpreadRule fragmentRule <- rule =
        applyToChildren |> fragmentRule fragmentSpread'
    | otherwise = applyToChildren
  where
    applyToChildren = directives context rule directives'

directives :: Traversable t
    => forall m
    . Validation m
    -> ApplyRule m (t Full.Directive)
directives context rule directives'
    | Validation.DirectivesRule directivesRule <- rule =
        applyToChildren |> directivesRule directiveList
    | otherwise = applyToChildren
  where
    directiveList = toList directives'
    applyToChildren = foldMap (directive context rule) directiveList

directive :: forall m. Validation m -> ApplyRule m Full.Directive
directive _ (Validation.ArgumentsRule _ argumentsRule) directive' =
    pure $ argumentsRule directive'
directive context rule (Full.Directive directiveName arguments' _) =
    let argumentTypes = maybe HashMap.empty directiveArguments
            $ HashMap.lookup directiveName (Validation.directives context)
     in arguments rule argumentTypes arguments'
  where
    directiveArguments (Schema.Directive _ _ argumentTypes) = argumentTypes
