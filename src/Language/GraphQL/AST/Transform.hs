{-# LANGUAGE TupleSections #-}

-- | After the document is parsed, before getting executed the AST is
--   transformed into a similar, simpler AST. This module is responsible for
--   this transformation.
module Language.GraphQL.AST.Transform
    ( document
    ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Language.GraphQL.AST as Full
import qualified Language.GraphQL.AST.Core as Core
import qualified Language.GraphQL.Schema as Schema

-- | Associates a fragment name with a list of 'Core.Field's.
type Fragments = HashMap Core.Name (NonEmpty Core.Selection)

data Replacement = Replacement
    { substitute :: Schema.Subs
    , fragments :: Fragments
    }

type TransformT a = ReaderT Replacement Maybe a

-- | Rewrites the original syntax tree into an intermediate representation used
-- for query execution.
document :: Schema.Subs -> Full.Document -> Maybe Core.Document
document subs doc = do
    fragmentMap <- foldr go (Just HashMap.empty) fragments'
    runReaderT (operations operations') $ Replacement subs fragmentMap
  where
    (fragments', operations') = foldr defragment ([], []) doc
    go fragDef (Just fragmentsMap) =
        runReaderT (fragmentDefinition fragDef) (Replacement subs fragmentsMap)
    go _ Nothing = Nothing

-- * Operation

-- TODO: Replace Maybe by MonadThrow CustomError
operations :: [Full.OperationDefinition] -> TransformT Core.Document
operations operations' = do
    coreOperations <- traverse operation operations'
    lift $ NonEmpty.nonEmpty coreOperations

operation :: Full.OperationDefinition -> TransformT Core.Operation
operation (Full.OperationSelectionSet sels) =
    operation $ Full.OperationDefinition Full.Query mempty mempty mempty sels
-- TODO: Validate Variable definitions with substituter
operation (Full.OperationDefinition Full.Query name _vars _dirs sels) =
    Core.Query name <$> appendSelection sels
operation (Full.OperationDefinition Full.Mutation name _vars _dirs sels) =
    Core.Mutation name <$> appendSelection sels

selection ::
    Full.Selection ->
    TransformT (Either (NonEmpty Core.Selection) Core.Selection)
selection (Full.SelectionField fld) = Right . Core.SelectionField <$> field fld
selection (Full.SelectionFragmentSpread (Full.FragmentSpread name _)) = do
    fragments' <- asks fragments
    lift $ Left <$> HashMap.lookup name fragments'
selection (Full.SelectionInlineFragment fragment)
    | (Full.InlineFragment (Just typeCondition) _ selectionSet) <- fragment
        = Right
        . Core.SelectionFragment
        . Core.Fragment typeCondition
        <$> appendSelection selectionSet
    | (Full.InlineFragment Nothing _ selectionSet) <- fragment
        = Left <$> appendSelection selectionSet

-- * Fragment replacement

-- | Extract fragments into a single 'HashMap' and operation definitions.
defragment ::
    Full.Definition ->
    ([Full.FragmentDefinition], [Full.OperationDefinition]) ->
    ([Full.FragmentDefinition], [Full.OperationDefinition])
defragment (Full.DefinitionOperation op) (fragments', operations') =
    (fragments', op : operations')
defragment (Full.DefinitionFragment fragDef) (fragments', operations') =
    (fragDef : fragments', operations')

fragmentDefinition :: Full.FragmentDefinition -> TransformT Fragments
fragmentDefinition (Full.FragmentDefinition name _tc _dirs sels) = do
    newValue <- emitValue
    fragments' <- asks fragments
    lift . Just $ HashMap.insert name newValue fragments'
  where
    emitValue = do
        selections <- traverse selection sels
        pure $ selections >>= either id pure

field :: Full.Field -> TransformT Core.Field
field (Full.Field a n args _dirs sels) = do
    arguments <- traverse argument args
    selection' <- appendSelectionOpt sels
    return $ Core.Field a n arguments selection'

argument :: Full.Argument -> TransformT Core.Argument
argument (Full.Argument n v) = Core.Argument n <$> value v

value :: Full.Value -> TransformT Core.Value
value (Full.Variable n) = do
    substitute' <- asks substitute
    lift $ substitute' n
value (Full.Int i) = pure $ Core.Int i
value (Full.Float f) = pure $ Core.Float f
value (Full.String x) = pure $ Core.String x
value (Full.Boolean b) = pure $ Core.Boolean b
value Full.Null = pure   Core.Null
value (Full.Enum e) = pure $ Core.Enum e
value (Full.List l) =
    Core.List <$> traverse value l
value (Full.Object o) =
    Core.Object . HashMap.fromList <$> traverse objectField o

objectField :: Full.ObjectField -> TransformT (Core.Name, Core.Value)
objectField (Full.ObjectField n v) = (n,) <$> value v

appendSelectionOpt ::
    Traversable t =>
    t Full.Selection ->
    TransformT [Core.Selection]
appendSelectionOpt = foldM go []
  where
    go acc sel = append acc <$> selection sel
    append acc (Left list) = NonEmpty.toList list <> acc
    append acc (Right one) = one : acc

appendSelection ::
    NonEmpty Full.Selection ->
    TransformT (NonEmpty Core.Selection)
appendSelection fullSelection = do
    coreSelection <-appendSelectionOpt fullSelection
    lift $ NonEmpty.nonEmpty coreSelection
