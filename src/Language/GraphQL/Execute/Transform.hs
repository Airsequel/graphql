{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | After the document is parsed, before getting executed the AST is
--   transformed into a similar, simpler AST. This module is responsible for
--   this transformation.
module Language.GraphQL.Execute.Transform
    ( Document(..)
    , OperationDefinition(..)
    , document
    , operation
    ) where

import Control.Monad (foldM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT, gets, modify)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Sequence (Seq, (<|), (><))
import qualified Language.GraphQL.AST as Full
import qualified Language.GraphQL.AST.Core as Core
import qualified Language.GraphQL.Schema as Schema
import qualified Language.GraphQL.Type.Directive as Directive

-- | Associates a fragment name with a list of 'Core.Field's.
data Replacement = Replacement
    { fragments :: HashMap Core.Name Core.Fragment
    , fragmentDefinitions :: HashMap Full.Name Full.FragmentDefinition
    }

type TransformT a = StateT Replacement (ReaderT Schema.Subs Maybe) a

liftJust :: forall a. a -> TransformT a
liftJust = lift . lift . Just

-- | GraphQL document is a non-empty list of operations.
data Document = Document
    (NonEmpty OperationDefinition)
    (HashMap Full.Name Full.FragmentDefinition)

data OperationDefinition = OperationDefinition
    Full.OperationType
    (Maybe Full.Name)
    [Full.VariableDefinition]
    [Full.Directive]
    Full.SelectionSet

-- | Rewrites the original syntax tree into an intermediate representation used
-- for query execution.
document :: Full.Document -> Maybe Document
document ast =
    let (operations, fragmentTable) = foldr defragment ([], HashMap.empty) ast
     in Document <$> NonEmpty.nonEmpty operations <*> pure fragmentTable
  where
    defragment definition (operations, fragments')
        | (Full.ExecutableDefinition executable) <- definition
        , (Full.DefinitionOperation operation') <- executable =
            (transform operation' : operations, fragments')
        | (Full.ExecutableDefinition executable) <- definition
        , (Full.DefinitionFragment fragment) <- executable
        , (Full.FragmentDefinition name _ _ _) <- fragment =
            (operations, HashMap.insert name fragment fragments')
    defragment _ acc = acc
    transform = \case
        Full.OperationDefinition type' name variables directives' selections ->
            OperationDefinition type' name variables directives' selections
        Full.SelectionSet selectionSet ->
            OperationDefinition Full.Query Nothing mempty mempty selectionSet

-- * Operation

operation
    :: HashMap Full.Name Full.FragmentDefinition
    -> Schema.Subs
    -> OperationDefinition
    -> Maybe Core.Operation
operation fragmentTable subs operationDefinition = flip runReaderT subs
    $ evalStateT (collectFragments >> transform operationDefinition)
    $ Replacement HashMap.empty fragmentTable
  where
    transform :: OperationDefinition -> TransformT Core.Operation
    transform (OperationDefinition Full.Query name _ _ sels) =
        Core.Query name <$> appendSelection sels
    transform (OperationDefinition Full.Mutation name _ _ sels) =
        Core.Mutation name <$> appendSelection sels

-- * Selection

selection ::
    Full.Selection ->
    TransformT (Either (Seq Core.Selection) Core.Selection)
selection (Full.Field alias name arguments' directives' selections) =
    maybe (Left mempty) (Right . Core.SelectionField) <$> do
        fieldArguments <- arguments arguments'
        fieldSelections <- appendSelection selections
        fieldDirectives <- Directive.selection <$> directives directives'
        let field' = Core.Field alias name fieldArguments fieldSelections
        pure $ field' <$ fieldDirectives
selection (Full.FragmentSpread name directives') =
    maybe (Left mempty) (Right . Core.SelectionFragment) <$> do
        spreadDirectives <- Directive.selection <$> directives directives'
        fragments' <- gets fragments
        fragment <- maybe lookupDefinition liftJust (HashMap.lookup name fragments')
        pure $ fragment <$ spreadDirectives
  where
    lookupDefinition = do
        fragmentDefinitions' <- gets fragmentDefinitions
        found <- lift . lift $ HashMap.lookup name fragmentDefinitions'
        fragmentDefinition found
selection (Full.InlineFragment type' directives' selections) = do
    fragmentDirectives <- Directive.selection <$> directives directives'
    case fragmentDirectives of
        Nothing -> pure $ Left mempty
        _ -> do
            fragmentSelectionSet <- appendSelection selections
            pure $ maybe Left selectionFragment type' fragmentSelectionSet
  where
    selectionFragment typeName = Right
        . Core.SelectionFragment
        . Core.Fragment typeName

appendSelection ::
    Traversable t =>
    t Full.Selection ->
    TransformT (Seq Core.Selection)
appendSelection = foldM go mempty
  where
    go acc sel = append acc <$> selection sel
    append acc (Left list) = list >< acc
    append acc (Right one) = one <| acc

directives :: [Full.Directive] -> TransformT [Core.Directive]
directives = traverse directive
  where
    directive (Full.Directive directiveName directiveArguments) =
        Core.Directive directiveName <$> arguments directiveArguments

-- * Fragment replacement

-- | Extract fragment definitions into a single 'HashMap'.
collectFragments :: TransformT ()
collectFragments = do
    fragDefs <- gets fragmentDefinitions
    let nextValue = head $ HashMap.elems fragDefs
    unless (HashMap.null fragDefs) $ do
        _ <- fragmentDefinition nextValue
        collectFragments

fragmentDefinition ::
    Full.FragmentDefinition ->
    TransformT Core.Fragment
fragmentDefinition (Full.FragmentDefinition name type' _ selections) = do
    modify deleteFragmentDefinition
    fragmentSelection <- appendSelection selections
    let newValue = Core.Fragment type' fragmentSelection
    modify $ insertFragment newValue
    liftJust newValue
  where
    deleteFragmentDefinition (Replacement fragments' fragmentDefinitions') =
        Replacement fragments' $ HashMap.delete name fragmentDefinitions'
    insertFragment newValue (Replacement fragments' fragmentDefinitions') =
        let newFragments = HashMap.insert name newValue fragments'
         in Replacement newFragments fragmentDefinitions'

arguments :: [Full.Argument] -> TransformT Core.Arguments
arguments = fmap Core.Arguments . foldM go HashMap.empty
  where
    go arguments' (Full.Argument name value') = do
        substitutedValue <- value value'
        return $ HashMap.insert name substitutedValue arguments'

value :: Full.Value -> TransformT Core.Value
value (Full.Variable name) = lift (asks $ HashMap.lookup name) >>= lift . lift
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
objectField (Full.ObjectField name value') = (name,) <$> value value'
