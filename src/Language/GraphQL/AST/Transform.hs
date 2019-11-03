{-# LANGUAGE TupleSections #-}

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
import qualified Language.GraphQL.AST as Full
import qualified Language.GraphQL.AST.Core as Core
import qualified Language.GraphQL.Schema as Schema

-- | Associates a fragment name with a list of 'Core.Field's.
type Fragments = HashMap Core.Name (NonEmpty Core.Field)

-- | Rewrites the original syntax tree into an intermediate representation used
-- for query execution.
document :: Schema.Subs -> Full.Document -> Maybe Core.Document
document subs doc =
    case fragments of
      Just fragments' -> operations subs fragments' operations'
      Nothing -> Nothing
  where
    (fragments, operations') = foldr (defrag subs) (Just HashMap.empty, [])
        $ NonEmpty.toList doc

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
    Core.Query name <$> appendSelection subs fragments sels
operation subs fragments (Full.OperationDefinition Full.Mutation name _vars _dirs sels) =
    Core.Mutation name <$> appendSelection subs fragments sels

selection ::
    Schema.Subs ->
    Fragments ->
    Full.Selection ->
    Maybe (Either (NonEmpty Core.Selection) Core.Selection)
selection subs fragments (Full.SelectionField fld)
    = Right . Core.SelectionField <$> field subs fragments fld
selection _ fragments (Full.SelectionFragmentSpread (Full.FragmentSpread name _))
    = Left . fmap Core.SelectionField <$> HashMap.lookup name fragments
selection subs fragments (Full.SelectionInlineFragment fragment)
    | (Full.InlineFragment (Just typeCondition) _ selectionSet) <- fragment
        = Right
        . Core.SelectionFragment
        . Core.Fragment typeCondition
        <$> appendSelection subs fragments selectionSet
    | (Full.InlineFragment Nothing _ selectionSet) <- fragment
        = Left <$> appendSelection subs fragments selectionSet

-- * Fragment replacement

-- | Extract fragments into a single 'HashMap' and operation definitions.
defrag ::
    Schema.Subs ->
    Full.Definition ->
    (Maybe Fragments, [Full.OperationDefinition]) ->
    (Maybe Fragments, [Full.OperationDefinition])
defrag _ (Full.DefinitionOperation op) (fragments, operations') =
    (fragments, op : operations')
defrag subs (Full.DefinitionFragment fragDef) (Just fragments, operations') =
    (fragmentDefinition subs fragments fragDef, operations')
defrag _ _ (Nothing, operations') =
    (Nothing, operations')

fragmentDefinition ::
    Schema.Subs ->
    Fragments ->
    Full.FragmentDefinition ->
    Maybe Fragments
fragmentDefinition subs fragments (Full.FragmentDefinition name _tc _dirs sels) = do
    emitted <- emitValue
    newValue <- traverse extractField emitted
    Just $ HashMap.insert name newValue fragments
  where
    emitValue :: Maybe (NonEmpty Core.Selection)
    emitValue = do
        selections <- traverse (selection subs fragments) sels
        pure $ selections >>= either id pure
    extractField :: Core.Selection -> Maybe Core.Field
    extractField (Core.SelectionField field') = Just field'
    extractField _ = Nothing -- Fragments within fragments are not supported yet

field :: Schema.Subs -> Fragments -> Full.Field -> Maybe Core.Field
field subs fragments (Full.Field a n args _dirs sels) =
    Core.Field a n (fold $ argument subs `traverse` args)
    <$> appendSelectionOpt subs fragments sels

argument :: Schema.Subs -> Full.Argument -> Maybe Core.Argument
argument subs (Full.Argument n v) = Core.Argument n <$> value subs v

value :: Schema.Subs -> Full.Value -> Maybe Core.Value
value subs (Full.Variable n) = subs n
value _    (Full.Int      i) = pure $ Core.Int i
value _    (Full.Float    f) = pure $ Core.Float f
value _    (Full.String   x) = pure $ Core.String x
value _    (Full.Boolean  b) = pure $ Core.Boolean b
value _     Full.Null        = pure   Core.Null
value _    (Full.Enum     e) = pure $ Core.Enum e
value subs (Full.List     l) =
    Core.List <$> traverse (value subs) l
value subs (Full.Object   o) =
    Core.Object . HashMap.fromList <$> traverse (objectField subs) o

objectField :: Schema.Subs -> Full.ObjectField -> Maybe (Core.Name, Core.Value)
objectField subs (Full.ObjectField n v) = (n,) <$> value subs v

appendSelectionOpt ::
    Traversable t =>
    Schema.Subs ->
    Fragments ->
    t Full.Selection ->
    Maybe [Core.Selection]
appendSelectionOpt subs fragments = foldr go (Just [])
  where
    go :: Full.Selection -> Maybe [Core.Selection] -> Maybe [Core.Selection]
    go _ Nothing = Nothing
    go sel (Just acc) = append acc <$> selection subs fragments sel
    append acc (Left list) = NonEmpty.toList list <> acc
    append acc (Right one) = one : acc

appendSelection ::
    Schema.Subs ->
    Fragments ->
    NonEmpty Full.Selection ->
    Maybe (NonEmpty Core.Selection)
appendSelection subs fragments fullSelection = do
    coreSelection <-appendSelectionOpt subs fragments fullSelection
    NonEmpty.nonEmpty coreSelection
