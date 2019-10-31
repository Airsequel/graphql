{-# LANGUAGE OverloadedStrings #-}

-- | After the document is parsed, before getting executed the AST is
--   transformed into a similar, simpler AST. This module is responsible for
--   this transformation.
module Language.GraphQL.AST.Transform
    ( document
    ) where

import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Language.GraphQL.AST as Full
import qualified Language.GraphQL.AST.Core as Core
import qualified Language.GraphQL.Schema as Schema

-- | Associates a fragment name with a list of 'Core.Field's.
type Fragments = HashMap Core.Name [Core.Field]

-- | Rewrites the original syntax tree into an intermediate representation used
-- for query execution.
document :: Schema.Subs -> Full.Document -> Maybe Core.Document
document subs doc = operations subs fragments operations'
  where
    (fragments, operations') = foldr (defrag subs) mempty
        $ NonEmpty.toList doc

extractFragment :: Fragments -> Core.Name -> [Core.Selection]
extractFragment fragments name = Core.SelectionField
    <$> fromMaybe mempty (HashMap.lookup name fragments)

-- * Operation

-- TODO: Replace Maybe by MonadThrow CustomError
operations ::
    Schema.Subs ->
    Fragments ->
    [Full.OperationDefinition] ->
    Maybe Core.Document
operations subs fragments operations' = do
    coreOperations <- traverse (operation subs fragments) operations'
    NonEmpty.nonEmpty coreOperations

operation ::
    Schema.Subs ->
    Fragments ->
    Full.OperationDefinition ->
    Maybe Core.Operation
operation subs fragments (Full.OperationSelectionSet sels) =
  operation subs fragments $ Full.OperationDefinition Full.Query mempty mempty mempty sels
-- TODO: Validate Variable definitions with substituter
operation subs fragments (Full.OperationDefinition Full.Query name _vars _dirs sels) =
    pure $ Core.Query name $ appendSelection subs fragments sels
operation subs fragments (Full.OperationDefinition Full.Mutation name _vars _dirs sels) =
    pure $ Core.Mutation name $ appendSelection subs fragments sels

selection ::
    Schema.Subs ->
    Fragments ->
    Full.Selection ->
    Either [Core.Selection] Core.Selection
selection subs fragments (Full.SelectionField fld)
    = Right $ Core.SelectionField $ field subs fragments fld
selection _ fragments (Full.SelectionFragmentSpread (Full.FragmentSpread name _))
    = Left $ extractFragment fragments name
selection subs fragments (Full.SelectionInlineFragment fragment)
    | (Full.InlineFragment (Just typeCondition) _ selectionSet) <- fragment
        = Right
        $ Core.SelectionFragment
        $ Core.Fragment typeCondition
        $ appendSelection subs fragments selectionSet
    | (Full.InlineFragment Nothing _ selectionSet) <- fragment
        = Left $ NonEmpty.toList $ appendSelection subs fragments selectionSet

-- * Fragment replacement

-- | Extract fragments into a single 'HashMap' and operation definitions.
defrag ::
    Schema.Subs ->
    Full.Definition ->
    (Fragments, [Full.OperationDefinition]) ->
    (Fragments, [Full.OperationDefinition])
defrag _ (Full.DefinitionOperation op) (fragments, operations') =
    (fragments, op : operations')
defrag subs (Full.DefinitionFragment fragDef) (fragments, operations') =
    (fragmentDefinition subs fragments fragDef, operations')

fragmentDefinition ::
    Schema.Subs ->
    Fragments ->
    Full.FragmentDefinition ->
    Fragments
fragmentDefinition subs fragments (Full.FragmentDefinition name _tc _dirs sels) =
    HashMap.insert name (extractField <$> emitValue) fragments
  where
    emitValue = do
        selections <- NonEmpty.toList $ selection subs mempty <$> sels
        either id pure selections
    extractField (Core.SelectionField field') = field'
    extractField _ = error "Fragments within fragments are not supported yet"

field :: Schema.Subs -> Fragments -> Full.Field -> Core.Field
field subs fragments (Full.Field a n args _dirs sels) =
    Core.Field a n (fold $ argument subs `traverse` args) (foldr go mempty sels)
  where
    go :: Full.Selection -> [Core.Selection] -> [Core.Selection]
    go (Full.SelectionFragmentSpread (Full.FragmentSpread name _dirs)) =
        (extractFragment fragments name <>)
    go sel = (either id pure (selection subs fragments sel) <>)

argument :: Schema.Subs -> Full.Argument -> Maybe Core.Argument
argument subs (Full.Argument n v) = Core.Argument n <$> value subs v

value :: Schema.Subs -> Full.Value -> Maybe Core.Value
value subs (Full.ValueVariable n) = subs n
value _    (Full.ValueInt      i) = pure $ Core.ValueInt i
value _    (Full.ValueFloat    f) = pure $ Core.ValueFloat f
value _    (Full.ValueString   x) = pure $ Core.ValueString x
value _    (Full.ValueBoolean  b) = pure $ Core.ValueBoolean b
value _     Full.ValueNull        = pure   Core.ValueNull
value _    (Full.ValueEnum     e) = pure $ Core.ValueEnum e
value subs (Full.ValueList     l) =
  Core.ValueList   <$> traverse (value subs) l
value subs (Full.ValueObject   o) =
  Core.ValueObject <$> traverse (objectField subs) o

objectField :: Schema.Subs -> Full.ObjectField -> Maybe Core.ObjectField
objectField subs (Full.ObjectField n v) = Core.ObjectField n <$> value subs v

appendSelection ::
    Schema.Subs ->
    Fragments ->
    NonEmpty Full.Selection ->
    NonEmpty Core.Selection
appendSelection subs fragments = NonEmpty.fromList
    . foldr (either (++) (:) . selection subs fragments) []
