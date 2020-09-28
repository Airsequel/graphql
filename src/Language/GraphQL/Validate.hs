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
import Language.GraphQL.AST.Document
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
    = HashMap Name (Schema.Type m)
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
    -> Document
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
    -> Definition
    -> Seq (Validation.RuleT m)
    -> Seq (Validation.RuleT m)
definition (Validation.DefinitionRule rule) _ definition' accumulator =
    accumulator |> rule definition'
definition rule context (ExecutableDefinition definition') accumulator =
    accumulator >< executableDefinition rule context definition'
definition rule _ (TypeSystemDefinition typeSystemDefinition' _) accumulator =
    accumulator >< typeSystemDefinition rule typeSystemDefinition'
definition rule _ (TypeSystemExtension extension _) accumulator =
    accumulator >< typeSystemExtension rule extension

typeSystemExtension :: forall m. ApplyRule m TypeSystemExtension
typeSystemExtension rule = \case
    SchemaExtension extension -> schemaExtension rule extension
    TypeExtension extension -> typeExtension rule extension

typeExtension :: forall m. ApplyRule m TypeExtension
typeExtension rule = \case
    ScalarTypeExtension _ directives' -> directives rule directives'
    ObjectTypeFieldsDefinitionExtension _ _ directives' fields ->
        directives rule directives' >< foldMap (fieldDefinition rule) fields
    ObjectTypeDirectivesExtension _ _ directives' -> directives rule directives'
    ObjectTypeImplementsInterfacesExtension _ _ -> mempty
    InterfaceTypeFieldsDefinitionExtension _ directives' fields ->
        directives rule directives' >< foldMap (fieldDefinition rule) fields
    InterfaceTypeDirectivesExtension _ directives' ->
        directives rule directives'
    UnionTypeUnionMemberTypesExtension _ directives' _ ->
        directives rule directives'
    UnionTypeDirectivesExtension _ directives' -> directives rule directives'
    EnumTypeEnumValuesDefinitionExtension _ directives' values ->
        directives rule directives' >< foldMap (enumValueDefinition rule) values
    EnumTypeDirectivesExtension _ directives' -> directives rule directives'
    InputObjectTypeInputFieldsDefinitionExtension _ directives' fields
        -> directives rule directives'
        >< foldMap (inputValueDefinition rule) fields
    InputObjectTypeDirectivesExtension _ directives' ->
        directives rule directives'

schemaExtension :: forall m. ApplyRule m SchemaExtension
schemaExtension rule = \case
    SchemaOperationExtension directives' _ -> directives rule directives'
    SchemaDirectivesExtension directives' -> directives rule directives'

executableDefinition :: forall m
    . Validation.Rule m
    -> Validation m
    -> ExecutableDefinition
    -> Seq (Validation.RuleT m)
executableDefinition rule context (DefinitionOperation operation) =
    operationDefinition rule context operation
executableDefinition rule context (DefinitionFragment fragment) =
    fragmentDefinition rule context fragment

typeSystemDefinition :: forall m. ApplyRule m TypeSystemDefinition
typeSystemDefinition rule = \case
    SchemaDefinition directives' _ -> directives rule directives'
    TypeDefinition typeDefinition' -> typeDefinition rule typeDefinition'
    DirectiveDefinition _ _ arguments' _ -> argumentsDefinition rule arguments'

typeDefinition :: forall m. ApplyRule m TypeDefinition
typeDefinition rule = \case
    ScalarTypeDefinition _ _ directives' -> directives rule directives'
    ObjectTypeDefinition _ _ _ directives' fields ->
        directives rule directives' >< foldMap (fieldDefinition rule) fields
    InterfaceTypeDefinition _ _ directives' fields ->
        directives rule directives' >< foldMap (fieldDefinition rule) fields
    UnionTypeDefinition _ _ directives' _ -> directives rule directives'
    EnumTypeDefinition _ _ directives' values ->
        directives rule directives' >< foldMap (enumValueDefinition rule) values
    InputObjectTypeDefinition _ _ directives' fields
        -> directives rule directives'
        <> foldMap (inputValueDefinition rule) fields

enumValueDefinition :: forall m. ApplyRule m EnumValueDefinition
enumValueDefinition rule (EnumValueDefinition _ _ directives') =
    directives rule directives'

fieldDefinition :: forall m. ApplyRule m FieldDefinition
fieldDefinition rule (FieldDefinition _ _ arguments' _ directives') =
    directives rule directives' >< argumentsDefinition rule arguments'

argumentsDefinition :: forall m. ApplyRule m ArgumentsDefinition
argumentsDefinition rule (ArgumentsDefinition definitions) =
    foldMap (inputValueDefinition rule) definitions

inputValueDefinition :: forall m. ApplyRule m InputValueDefinition
inputValueDefinition rule (InputValueDefinition _ _ _ _ directives') =
    directives rule directives'

operationDefinition :: forall m
    . Validation.Rule m
    -> Validation m
    -> OperationDefinition
    -> Seq (Validation.RuleT m)
operationDefinition rule context operation
    | Validation.OperationDefinitionRule operationRule <- rule =
        pure $ operationRule operation
    | Validation.VariablesRule variablesRule <- rule
    , OperationDefinition _ _ variables _ _ _ <- operation
        = Seq.fromList (variableDefinition rule <$> variables)
        |> variablesRule variables
    | SelectionSet selections _ <- operation =
        selectionSet types' rule (getRootType Query) selections
    | OperationDefinition operationType _ _ directives' selections _  <- operation
        = selectionSet types' rule (getRootType operationType) selections
        >< directives rule directives'
  where
    types' = Validation.types context
    getRootType Query =
       Just $ Out.NamedObjectType $ Schema.query $ Validation.schema context
    getRootType Mutation =
       Out.NamedObjectType <$> Schema.mutation (Validation.schema context)
    getRootType Subscription =
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
    . Validation.Rule m
    -> VariableDefinition
    -> Validation.RuleT m
variableDefinition (Validation.ValueRule _ rule) (VariableDefinition _ _ value _) =
    maybe (lift mempty) rule value
variableDefinition _ _ = lift mempty

fragmentDefinition :: forall m
    . Validation.Rule m
    -> Validation m
    -> FragmentDefinition
    -> Seq (Validation.RuleT m)
fragmentDefinition (Validation.FragmentDefinitionRule rule) _ definition' =
    pure $ rule definition'
fragmentDefinition rule context definition'
    | FragmentDefinition _ typeCondition directives' selections _ <- definition'
    , Validation.FragmentRule definitionRule _ <- rule
        = applyToChildren typeCondition directives' selections
        |> definitionRule definition'
    | FragmentDefinition _ typeCondition directives' selections _ <- definition'
        = applyToChildren typeCondition directives' selections
  where
    types' = Validation.types context
    applyToChildren typeCondition directives' selections
        = selectionSet types' rule (lookupType' typeCondition) selections
        >< directives rule directives'
    lookupType' = flip lookupType types'

lookupType :: forall m
    . TypeCondition
    -> HashMap Name (Schema.Type m)
    -> Maybe (Out.Type m)
lookupType typeCondition types' = HashMap.lookup typeCondition types'
    >>= typeToOut

selectionSet :: Traversable t => forall m. ApplySelectionRule m (t Selection)
selectionSet types' rule = foldMap . selection types' rule

selection :: forall m. ApplySelectionRule m Selection
selection types' rule objectType selection'
    | Validation.SelectionRule selectionRule <- rule =
        applyToChildren |> selectionRule objectType selection'
    | otherwise = applyToChildren
  where
    applyToChildren =
        case selection' of
            FieldSelection field' -> field types' rule objectType field'
            InlineFragmentSelection inlineFragment' ->
                inlineFragment types' rule objectType inlineFragment'
            FragmentSpreadSelection fragmentSpread' ->
                fragmentSpread rule fragmentSpread'

field :: forall m. ApplySelectionRule m Field
field types' rule objectType field' = go field'
  where
    go (Field _ fieldName _ _ _ _)
        | Validation.FieldRule fieldRule <- rule =
            applyToChildren fieldName |> fieldRule objectType field'
        | Validation.ArgumentsRule argumentsRule _  <- rule =
            applyToChildren fieldName |> argumentsRule objectType field'
        | otherwise = applyToChildren fieldName
    typeFieldType (Out.Field _ type' _) = type'
    applyToChildren fieldName =
        let Field _ _ arguments' directives' selections _ = field'
            fieldType = objectType
               >>= fmap typeFieldType . lookupTypeField fieldName
         in selectionSet types' rule fieldType selections
            >< directives rule directives'
            >< arguments rule arguments'

arguments :: forall m. ApplyRule m [Argument]
arguments = (.) Seq.fromList . fmap . argument

argument :: forall m. Validation.Rule m -> Argument -> Validation.RuleT m
argument (Validation.ValueRule rule _) (Argument _ (Node value _) _) =
    rule value
argument _ _ = lift mempty

inlineFragment :: forall m. ApplySelectionRule m InlineFragment
inlineFragment types' rule objectType inlineFragment' = go inlineFragment'
  where
    go (InlineFragment optionalType directives' selections _)
        | Validation.FragmentRule _ fragmentRule <- rule
            = applyToChildren (refineTarget optionalType) directives' selections
            |> fragmentRule inlineFragment'
        | otherwise = applyToChildren (refineTarget optionalType) directives' selections
    refineTarget (Just typeCondition) = lookupType typeCondition types'
    refineTarget Nothing = objectType
    applyToChildren objectType' directives' selections
        = selectionSet types' rule objectType' selections
        >< directives rule directives'

fragmentSpread :: forall m. ApplyRule m FragmentSpread
fragmentSpread rule fragmentSpread'@(FragmentSpread _ directives' _)
    | Validation.FragmentSpreadRule fragmentRule <- rule =
        applyToChildren |> fragmentRule fragmentSpread'
    | otherwise = applyToChildren
  where
    applyToChildren = directives rule directives'

directives :: Traversable t => forall m. ApplyRule m (t Directive)
directives rule directives'
    | Validation.DirectivesRule directivesRule <- rule =
        applyToChildren |> directivesRule directiveList
    | otherwise = applyToChildren
  where
    directiveList = toList directives'
    applyToChildren = foldMap (directive rule) directiveList

directive :: forall m. ApplyRule m Directive
directive (Validation.ArgumentsRule _ argumentsRule) directive' =
    pure $ argumentsRule directive'
directive rule (Directive _ arguments' _) = arguments rule arguments'
