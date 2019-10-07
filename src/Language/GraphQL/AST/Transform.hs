{-# LANGUAGE OverloadedStrings #-}

-- | After the document is parsed, before getting executed the AST is
--   transformed into a similar, simpler AST. This module is responsible for
--   this transformation.
module Language.GraphQL.AST.Transform
    ( document
    ) where

import Control.Applicative (empty)
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (fold, foldMap)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid (Alt(Alt,getAlt), (<>))
import qualified Language.GraphQL.AST as Full
import qualified Language.GraphQL.AST.Core as Core
import qualified Language.GraphQL.Schema as Schema

-- | Replaces a fragment name by a list of 'Core.Field'. If the name doesn't
--   match an empty list is returned.
type Fragmenter = Core.Name -> [Core.Field]

-- | Rewrites the original syntax tree into an intermediate representation used
-- for query execution.
document :: Schema.Subs -> Full.Document -> Maybe Core.Document
document subs doc = operations subs fr ops
  where
    (fr, ops) = first foldFrags
              . partitionEithers
              . NonEmpty.toList
              $ defrag subs
            <$> doc

    foldFrags :: [Fragmenter] -> Fragmenter
    foldFrags fs n = getAlt $ foldMap (Alt . ($ n)) fs

-- * Operation

-- TODO: Replace Maybe by MonadThrow CustomError
operations
  :: Schema.Subs
  -> Fragmenter
  -> [Full.OperationDefinition]
  -> Maybe Core.Document
operations subs fr = NonEmpty.nonEmpty <=< traverse (operation subs fr)

operation
  :: Schema.Subs
  -> Fragmenter
  -> Full.OperationDefinition
  -> Maybe Core.Operation
operation subs fr (Full.OperationSelectionSet sels) =
  operation subs fr $ Full.OperationDefinition Full.Query empty empty empty sels
-- TODO: Validate Variable definitions with substituter
operation subs fr (Full.OperationDefinition operationType name _vars _dirs sels)
    = case operationType of
        Full.Query -> Core.Query name <$> node
        Full.Mutation -> Core.Mutation name <$> node
  where
    node = traverse (hush . selection subs fr) sels

selection
  :: Schema.Subs
  -> Fragmenter
  -> Full.Selection
  -> Either [Core.Selection] Core.Selection
selection subs fr (Full.SelectionField fld) =
  Right $ Core.SelectionField $ field subs fr fld
selection _    fr (Full.SelectionFragmentSpread (Full.FragmentSpread n _dirs)) =
  Left $ Core.SelectionField <$> fr n
selection subs fr  (Full.SelectionInlineFragment fragment)
    | (Full.InlineFragment (Just typeCondition) _ selectionSet) <- fragment
    = Right $ Core.SelectionFragment $ Core.Fragment typeCondition $ node selectionSet
    | otherwise = error "Inline fragments not supported yet"
  where
    node selections
        = NonEmpty.fromList
        $ foldr (appendSelection . selection subs fr) [] selections
    appendSelection (Left x) acc = x ++ acc
    appendSelection (Right x) acc = x : acc

-- * Fragment replacement

-- | Extract Fragments into a single Fragmenter function and a Operation
--   Definition.
defrag
  :: Schema.Subs
  -> Full.Definition
  -> Either Fragmenter Full.OperationDefinition
defrag _    (Full.DefinitionOperation op) =
  Right op
defrag subs (Full.DefinitionFragment fragDef) =
  Left $ fragmentDefinition subs fragDef

fragmentDefinition :: Schema.Subs -> Full.FragmentDefinition -> Fragmenter
fragmentDefinition subs (Full.FragmentDefinition name _tc _dirs sels) name'
    -- TODO: Support fragments within fragments. Fold instead of map.
    | name == name' = selection' <$> do
        selections <- NonEmpty.toList $ selection subs mempty <$> sels
        either id pure selections
    | otherwise = empty
  where
    selection' (Core.SelectionField field') = field'
    selection' _ = error "Inline fragments not supported yet"

field :: Schema.Subs -> Fragmenter -> Full.Field -> Core.Field
field subs fr (Full.Field a n args _dirs sels) =
    Core.Field a n (fold $ argument subs `traverse` args) (foldr go empty sels)
  where
    go :: Full.Selection -> [Core.Selection] -> [Core.Selection]
    go (Full.SelectionFragmentSpread (Full.FragmentSpread name _dirs)) = ((Core.SelectionField <$> fr name) <>)
    go sel = (either id pure (selection subs fr sel) <>)

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

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
