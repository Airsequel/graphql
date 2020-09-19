{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Sequence (Seq(..), (><), (|>))
import qualified Data.Sequence as Seq
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Internal
import Language.GraphQL.Type.Schema (Schema(..))
import Language.GraphQL.Validate.Rules
import Language.GraphQL.Validate.Validation

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
        join $ lift $ foldr (definition rule') Seq.empty document'

definition :: Rule m -> Definition -> Seq (RuleT m) -> Seq (RuleT m)
definition (DefinitionRule rule) definition' accumulator =
    accumulator |> rule definition'
definition rule (ExecutableDefinition executableDefinition') accumulator =
    accumulator >< executableDefinition rule executableDefinition'
definition rule (TypeSystemDefinition typeSystemDefinition' _) accumulator =
    accumulator >< typeSystemDefinition rule typeSystemDefinition'
definition rule (TypeSystemExtension extension _) accumulator =
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

executableDefinition :: Rule m -> ExecutableDefinition -> Seq (RuleT m)
executableDefinition rule (DefinitionOperation operation) =
    operationDefinition rule operation
executableDefinition rule (DefinitionFragment fragment) =
    fragmentDefinition rule fragment

typeSystemDefinition :: Rule m -> TypeSystemDefinition -> Seq (RuleT m)
typeSystemDefinition rule = \case
    SchemaDefinition directives' _ -> directives rule directives'
    TypeDefinition typeDefinition' -> typeDefinition rule typeDefinition'
    DirectiveDefinition _ _ arguments _ -> argumentsDefinition rule arguments

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
fieldDefinition rule (FieldDefinition _ _ arguments _ directives') =
    directives rule directives' >< argumentsDefinition rule arguments

argumentsDefinition :: Rule m -> ArgumentsDefinition -> Seq (RuleT m)
argumentsDefinition rule (ArgumentsDefinition definitions) =
    foldMap (inputValueDefinition rule) definitions

inputValueDefinition :: Rule m -> InputValueDefinition -> Seq (RuleT m)
inputValueDefinition rule (InputValueDefinition _ _ _ _ directives') =
    directives rule directives'

operationDefinition :: Rule m -> OperationDefinition -> Seq (RuleT m)
operationDefinition rule operation
    | OperationDefinitionRule operationRule <- rule =
        pure $ operationRule operation
    | VariablesRule variablesRule <- rule
    , OperationDefinition _ _ variables _ _ _ <- operation =
        pure $ variablesRule variables
    | SelectionSet selections _ <- operation = selectionSet rule selections
    | OperationDefinition _ _ _ directives' selections _  <- operation =
        selectionSet rule selections >< directives rule directives'

fragmentDefinition :: Rule m -> FragmentDefinition -> Seq (RuleT m)
fragmentDefinition (FragmentDefinitionRule rule) fragmentDefinition' =
    pure $ rule fragmentDefinition'
fragmentDefinition rule fragmentDefinition'@(FragmentDefinition _ _ directives' selections _)
    | FragmentRule definitionRule _ <- rule =
        applyToChildren |> definitionRule fragmentDefinition'
    | otherwise = applyToChildren
  where
    applyToChildren = selectionSet rule selections
        >< directives rule directives'

selectionSet :: Traversable t => Rule m -> t Selection -> Seq (RuleT m)
selectionSet = foldMap . selection

selection :: Rule m -> Selection -> Seq (RuleT m)
selection rule selection'
    | SelectionRule selectionRule <- rule =
        applyToChildren |> selectionRule selection'
    | otherwise = applyToChildren
  where
    applyToChildren =
        case selection' of
            FieldSelection field' -> field rule field'
            InlineFragmentSelection inlineFragment' ->
                inlineFragment rule inlineFragment'
            FragmentSpreadSelection fragmentSpread' ->
                fragmentSpread rule fragmentSpread'

field :: Rule m -> Field -> Seq (RuleT m)
field rule field'@(Field _ _ _ directives' selections _)
    | FieldRule fieldRule <- rule = applyToChildren |> fieldRule field'
    | ArgumentsRule fieldRule _  <- rule = applyToChildren |> fieldRule field'
    | otherwise = applyToChildren
  where
    applyToChildren = selectionSet rule selections >< directives rule directives'

inlineFragment :: Rule m -> InlineFragment -> Seq (RuleT m)
inlineFragment rule inlineFragment'@(InlineFragment _ directives' selections _)
    | FragmentRule _ fragmentRule <- rule =
        applyToChildren |> fragmentRule inlineFragment'
    | otherwise = applyToChildren
  where
    applyToChildren = selectionSet rule selections
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
    applyToChildren = Seq.fromList $ fmap (directive rule) directiveList

directive :: Rule m -> Directive -> RuleT m
directive (ArgumentsRule _ rule) = rule
directive _ = lift . const mempty
