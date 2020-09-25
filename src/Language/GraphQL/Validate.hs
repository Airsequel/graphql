{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | GraphQL validator.
module Language.GraphQL.Validate
    ( Error(..)
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
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Internal
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema (Schema(..))
import qualified Language.GraphQL.Type.Schema as Schema
import Language.GraphQL.Validate.Rules
import Language.GraphQL.Validate.Validation

type ApplyRule m a =
    HashMap Name (Schema.Type m) -> Rule m -> Maybe (Out.Type m) -> a -> Seq (RuleT m)

-- | Validates a document and returns a list of found errors. If the returned
-- list is empty, the document is valid.
document :: forall m. Schema m -> [Rule m] -> Document -> Seq Error
document schema' rules' document' =
    runReaderT reader context
  where
    context = Validation
        { ast = document'
        , schema = schema'
        , types = collectReferencedTypes schema'
        }
    reader = do
        rule' <- lift $ Seq.fromList rules'
        join $ lift $ foldr (definition rule' context) Seq.empty document'

definition :: Rule m
    -> Validation m
    -> Definition
    -> Seq (RuleT m)
    -> Seq (RuleT m)
definition (DefinitionRule rule) _ definition' accumulator =
    accumulator |> rule definition'
definition rule context (ExecutableDefinition definition') accumulator =
    accumulator >< executableDefinition rule context definition'
definition rule _ (TypeSystemDefinition typeSystemDefinition' _) accumulator =
    accumulator >< typeSystemDefinition rule typeSystemDefinition'
definition rule _ (TypeSystemExtension extension _) accumulator =
    accumulator >< typeSystemExtension rule extension

typeSystemExtension :: Rule m -> TypeSystemExtension -> Seq (RuleT m)
typeSystemExtension rule = \case
    SchemaExtension extension -> schemaExtension rule extension
    TypeExtension extension -> typeExtension rule extension

typeExtension :: Rule m -> TypeExtension -> Seq (RuleT m)
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

schemaExtension :: Rule m -> SchemaExtension -> Seq (RuleT m)
schemaExtension rule = \case
    SchemaOperationExtension directives' _ -> directives rule directives'
    SchemaDirectivesExtension directives' -> directives rule directives'

executableDefinition :: Rule m
    -> Validation m
    -> ExecutableDefinition
    -> Seq (RuleT m)
executableDefinition rule context (DefinitionOperation operation) =
    operationDefinition rule context operation
executableDefinition rule context (DefinitionFragment fragment) =
    fragmentDefinition rule context fragment

typeSystemDefinition :: Rule m -> TypeSystemDefinition -> Seq (RuleT m)
typeSystemDefinition rule = \case
    SchemaDefinition directives' _ -> directives rule directives'
    TypeDefinition typeDefinition' -> typeDefinition rule typeDefinition'
    DirectiveDefinition _ _ arguments' _ -> argumentsDefinition rule arguments'

typeDefinition :: Rule m -> TypeDefinition -> Seq (RuleT m)
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

enumValueDefinition :: Rule m -> EnumValueDefinition -> Seq (RuleT m)
enumValueDefinition rule (EnumValueDefinition _ _ directives') =
    directives rule directives'

fieldDefinition :: Rule m -> FieldDefinition -> Seq (RuleT m)
fieldDefinition rule (FieldDefinition _ _ arguments' _ directives') =
    directives rule directives' >< argumentsDefinition rule arguments'

argumentsDefinition :: Rule m -> ArgumentsDefinition -> Seq (RuleT m)
argumentsDefinition rule (ArgumentsDefinition definitions) =
    foldMap (inputValueDefinition rule) definitions

inputValueDefinition :: Rule m -> InputValueDefinition -> Seq (RuleT m)
inputValueDefinition rule (InputValueDefinition _ _ _ _ directives') =
    directives rule directives'

operationDefinition :: Rule m
    -> Validation m
    -> OperationDefinition
    -> Seq (RuleT m)
operationDefinition rule context operation
    | OperationDefinitionRule operationRule <- rule =
        pure $ operationRule operation
    | VariablesRule variablesRule <- rule
    , OperationDefinition _ _ variables _ _ _ <- operation
        = Seq.fromList (variableDefinition rule <$> variables)
        |> variablesRule variables
    | SelectionSet selections _ <- operation =
        selectionSet types' rule (getRootType Query) selections
    | OperationDefinition operationType _ _ directives' selections _  <- operation
        = selectionSet types' rule (getRootType operationType) selections
        >< directives rule directives'
  where
    types' = types context
    getRootType Query = Just $ Out.NamedObjectType $ query $ schema context
    getRootType Mutation = Out.NamedObjectType <$> mutation (schema context)
    getRootType Subscription =
        Out.NamedObjectType <$> subscription (schema context)
        
typeToOut :: forall m. Schema.Type m -> Maybe (Out.Type m)
typeToOut (Schema.ObjectType objectType) =
    Just $ Out.NamedObjectType objectType
typeToOut (Schema.InterfaceType interfaceType) =
    Just $ Out.NamedInterfaceType interfaceType
typeToOut (Schema.UnionType unionType) = Just $ Out.NamedUnionType unionType
typeToOut (Schema.EnumType enumType) = Just $ Out.NamedEnumType enumType
typeToOut (Schema.ScalarType scalarType) = Just $ Out.NamedScalarType scalarType
typeToOut _ = Nothing

variableDefinition :: Rule m -> VariableDefinition -> RuleT m
variableDefinition (ValueRule _ rule) (VariableDefinition _ _ value _) =
    maybe (lift mempty) rule value
variableDefinition _ _ = lift mempty

fragmentDefinition :: forall m
    . Rule m
    -> Validation m
    -> FragmentDefinition
    -> Seq (RuleT m)
fragmentDefinition (FragmentDefinitionRule rule) _ definition' =
    pure $ rule definition'
fragmentDefinition rule context definition'
    | FragmentDefinition _ typeCondition directives' selections _ <- definition'
    , FragmentRule definitionRule _ <- rule
        = applyToChildren typeCondition directives' selections
        |> definitionRule definition'
    | FragmentDefinition _ typeCondition directives' selections _ <- definition'
        = applyToChildren typeCondition directives' selections
  where
    types' = types context
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

selectionSet :: Traversable t => forall m. ApplyRule m (t Selection)
selectionSet types' rule = foldMap . selection types' rule

selection :: forall m. ApplyRule m Selection
selection types' rule objectType selection'
    | SelectionRule selectionRule <- rule =
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

field :: forall m. ApplyRule m Field
field types' rule objectType field' = go field'
  where
    go (Field _ fieldName arguments' directives' selections _)
        | ArgumentsRule fieldRule _  <- rule
            = applyToChildren fieldName arguments' directives' selections
            |> fieldRule field'
        | otherwise =
            applyToChildren fieldName arguments' directives' selections
    applyToChildren fieldName arguments' directives' selections =
        let child = objectType >>= lookupTypeField fieldName
         in selectionSet types' rule child selections
            >< directives rule directives'
            >< arguments rule arguments'

arguments :: Rule m -> [Argument] -> Seq (RuleT m)
arguments = (.) Seq.fromList . fmap . argument

argument :: Rule m -> Argument -> RuleT m
argument (ValueRule rule _) (Argument _ (Node value _) _) = rule value
argument _ _ = lift mempty

inlineFragment :: forall m. ApplyRule m InlineFragment
inlineFragment types' rule objectType inlineFragment' = go inlineFragment'
  where
    go (InlineFragment optionalType directives' selections _)
        | FragmentRule _ fragmentRule <- rule
            = applyToChildren (refineTarget optionalType) directives' selections
            |> fragmentRule inlineFragment'
        | otherwise = applyToChildren (refineTarget optionalType) directives' selections
    refineTarget (Just typeCondition) = lookupType typeCondition types'
    refineTarget Nothing = objectType
    applyToChildren objectType' directives' selections
        = selectionSet types' rule objectType' selections
        >< directives rule directives'

fragmentSpread :: Rule m -> FragmentSpread -> Seq (RuleT m)
fragmentSpread rule fragmentSpread'@(FragmentSpread _ directives' _)
    | FragmentSpreadRule fragmentRule <- rule =
        applyToChildren |> fragmentRule fragmentSpread'
    | otherwise = applyToChildren
  where
    applyToChildren = directives rule directives'

directives :: Traversable t => Rule m -> t Directive -> Seq (RuleT m)
directives rule directives'
    | DirectivesRule directivesRule <- rule =
        applyToChildren |> directivesRule directiveList
    | otherwise = applyToChildren
  where
    directiveList = toList directives'
    applyToChildren = foldMap (directive rule) directiveList

directive :: Rule m -> Directive -> Seq (RuleT m)
directive (ArgumentsRule _ argumentsRule) directive' =
    pure $ argumentsRule directive'
directive rule (Directive _ arguments' _) = arguments rule arguments'
