{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.GraphQL.Execute.Execution
    ( coerceArgumentValues
    , collectFields
    , executeSelectionSet
    ) where

import Control.Monad.Catch (Exception(..), MonadCatch(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State (gets)
import Data.List.NonEmpty (NonEmpty(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq(..))
import qualified Data.Text as Text
import Language.GraphQL.AST (Name)
import Language.GraphQL.Error
import Language.GraphQL.Execute.Coerce
import Language.GraphQL.Execute.Internal
import Language.GraphQL.Execute.OrderedMap (OrderedMap)
import qualified Language.GraphQL.Execute.OrderedMap as OrderedMap
import qualified Language.GraphQL.Execute.Transform as Transform
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import qualified Language.GraphQL.Type.Internal as Internal
import Prelude hiding (null)

resolveFieldValue :: MonadCatch m
    => Type.Value
    -> Type.Subs
    -> Type.Resolve m
    -> CollectErrsT m Type.Value
resolveFieldValue result args resolver =
    catch (lift $ runReaderT resolver context) handleFieldError
  where
    handleFieldError :: MonadCatch m
        => ResolverException
        -> CollectErrsT m Type.Value
    handleFieldError e =
        addError Type.Null $ Error (Text.pack $ displayException e) [] []
    context = Type.Context
        { Type.arguments = Type.Arguments args
        , Type.values = result
        }

collectFields :: Monad m
    => Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> OrderedMap (NonEmpty (Transform.Field m))
collectFields objectType = foldl forEach OrderedMap.empty
  where
    forEach groupedFields (Transform.SelectionField field) =
        let responseKey = aliasOrName field
         in OrderedMap.insert responseKey (field :| []) groupedFields
    forEach groupedFields (Transform.SelectionFragment selectionFragment)
        | Transform.Fragment fragmentType fragmentSelectionSet <- selectionFragment
        , Internal.doesFragmentTypeApply fragmentType objectType =
            let fragmentGroupedFieldSet = collectFields objectType fragmentSelectionSet
             in groupedFields <> fragmentGroupedFieldSet
        | otherwise = groupedFields

aliasOrName :: forall m. Transform.Field m -> Name
aliasOrName (Transform.Field alias name _ _) = fromMaybe name alias

resolveAbstractType :: Monad m
    => Internal.AbstractType m
    -> Type.Subs
    -> CollectErrsT m (Maybe (Out.ObjectType m))
resolveAbstractType abstractType values'
    | Just (Type.String typeName) <- HashMap.lookup "__typename" values' = do
        types' <- gets types
        case HashMap.lookup typeName types' of
            Just (Internal.ObjectType objectType) ->
                if Internal.instanceOf objectType abstractType
                    then pure $ Just objectType
                    else pure Nothing
            _ -> pure Nothing
    | otherwise = pure Nothing

executeField :: (MonadCatch m, Serialize a)
    => Out.Resolver m
    -> Type.Value
    -> NonEmpty (Transform.Field m)
    -> CollectErrsT m a
executeField fieldResolver prev fields
    | Out.ValueResolver fieldDefinition resolver <- fieldResolver =
        executeField' fieldDefinition resolver
    | Out.EventStreamResolver fieldDefinition resolver _ <- fieldResolver =
        executeField' fieldDefinition resolver
  where
    executeField' fieldDefinition resolver = do
        let Out.Field _ fieldType argumentDefinitions = fieldDefinition
        let (Transform.Field _ _ arguments' _ :| []) = fields
        case coerceArgumentValues argumentDefinitions arguments' of
            Nothing -> addError null $ Error "Argument coercing failed." [] []
            Just argumentValues -> do
                answer <- resolveFieldValue prev argumentValues resolver
                completeValue fieldType fields answer

completeValue :: (MonadCatch m, Serialize a)
    => Out.Type m
    -> NonEmpty (Transform.Field m)
    -> Type.Value
    -> CollectErrsT m a
completeValue (Out.isNonNullType -> False) _ Type.Null = pure null
completeValue outputType@(Out.ListBaseType listType) fields (Type.List list)
    = traverse (completeValue listType fields) list
    >>= coerceResult outputType . List
completeValue outputType@(Out.ScalarBaseType _) _ (Type.Int int) =
    coerceResult outputType $ Int int
completeValue outputType@(Out.ScalarBaseType _) _ (Type.Boolean boolean) =
    coerceResult outputType $ Boolean boolean
completeValue outputType@(Out.ScalarBaseType _) _ (Type.Float float) =
    coerceResult outputType $ Float float
completeValue outputType@(Out.ScalarBaseType _) _ (Type.String string) =
    coerceResult outputType $ String string
completeValue outputType@(Out.EnumBaseType enumType) _ (Type.Enum enum) =
    let Type.EnumType _ _ enumMembers = enumType
     in if HashMap.member enum enumMembers
        then coerceResult outputType $ Enum enum
        else addError null $ Error "Enum value completion failed." [] []
completeValue (Out.ObjectBaseType objectType) fields result =
    executeSelectionSet result objectType $ mergeSelectionSets fields
completeValue (Out.InterfaceBaseType interfaceType) fields result
    | Type.Object objectMap <- result = do
        let abstractType = Internal.AbstractInterfaceType interfaceType
        concreteType <- resolveAbstractType abstractType objectMap
        case concreteType of
            Just objectType -> executeSelectionSet result objectType
                $ mergeSelectionSets fields
            Nothing -> addError null
                $ Error "Interface value completion failed." [] []
completeValue (Out.UnionBaseType unionType) fields result
    | Type.Object objectMap <- result = do
        let abstractType = Internal.AbstractUnionType unionType
        concreteType <- resolveAbstractType abstractType objectMap
        case concreteType of
            Just objectType -> executeSelectionSet result objectType
                $ mergeSelectionSets fields
            Nothing -> addError null
                $ Error "Union value completion failed." [] []
completeValue _ _ _ = addError null $ Error "Value completion failed." [] []

mergeSelectionSets :: MonadCatch m
    => NonEmpty (Transform.Field m)
    -> Seq (Transform.Selection m)
mergeSelectionSets = foldr forEach mempty
  where
    forEach (Transform.Field _ _ _ fieldSelectionSet) selectionSet =
        selectionSet <> fieldSelectionSet

coerceResult :: (MonadCatch m, Serialize a)
    => Out.Type m
    -> Output a
    -> CollectErrsT m a
coerceResult outputType result
    | Just serialized <- serialize outputType result = pure serialized
    | otherwise = addError null $ Error "Result coercion failed." [] []

-- | Takes an 'Out.ObjectType' and a list of 'Transform.Selection's and applies
-- each field to each 'Transform.Selection'. Resolves into a value containing
-- the resolved 'Transform.Selection', or a null value and error information.
executeSelectionSet :: (MonadCatch m, Serialize a)
    => Type.Value
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> CollectErrsT m a
executeSelectionSet result objectType@(Out.ObjectType _ _ _ resolvers) selectionSet = do
    let fields = collectFields objectType selectionSet
    resolvedValues <- OrderedMap.traverseMaybe forEach fields
    coerceResult (Out.NonNullObjectType objectType) $ Object resolvedValues
  where
    forEach fields@(field :| _) =
        let Transform.Field _ name _ _ = field
         in traverse (tryResolver fields) $ lookupResolver name
    lookupResolver = flip HashMap.lookup resolvers
    tryResolver fields resolver =
        executeField resolver result fields >>= lift . pure

coerceArgumentValues
    :: HashMap Name In.Argument
    -> HashMap Name Transform.Input
    -> Maybe Type.Subs
coerceArgumentValues argumentDefinitions argumentValues =
    HashMap.foldrWithKey forEach (pure mempty) argumentDefinitions
  where
    forEach variableName (In.Argument _ variableType defaultValue) =
        matchFieldValues coerceArgumentValue argumentValues variableName variableType defaultValue
    coerceArgumentValue inputType (Transform.Int integer) =
        coerceInputLiteral inputType (Type.Int integer)
    coerceArgumentValue inputType (Transform.Boolean boolean) =
        coerceInputLiteral inputType (Type.Boolean boolean)
    coerceArgumentValue inputType (Transform.String string) =
        coerceInputLiteral inputType (Type.String string)
    coerceArgumentValue inputType (Transform.Float float) =
        coerceInputLiteral inputType (Type.Float float)
    coerceArgumentValue inputType (Transform.Enum enum) =
        coerceInputLiteral inputType (Type.Enum enum)
    coerceArgumentValue inputType Transform.Null
        | In.isNonNullType inputType = Nothing
        | otherwise = coerceInputLiteral inputType Type.Null
    coerceArgumentValue (In.ListBaseType inputType) (Transform.List list) =
        let coerceItem = coerceInputLiteral inputType
         in Type.List <$> traverse coerceItem list
    coerceArgumentValue (In.InputObjectBaseType inputType) (Transform.Object object)
        | In.InputObjectType _ _ inputFields <- inputType = 
            let go = forEachField object
                resultMap = HashMap.foldrWithKey go (pure mempty) inputFields
             in Type.Object <$> resultMap
    coerceArgumentValue _ (Transform.Variable variable) = pure variable
    coerceArgumentValue _ _ = Nothing
    forEachField object variableName (In.InputField _ variableType defaultValue) =
        matchFieldValues coerceArgumentValue object variableName variableType defaultValue
