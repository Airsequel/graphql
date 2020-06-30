{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.GraphQL.Execute.Execution
    ( executeSelectionSet
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State (gets)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Language.GraphQL.AST (Name)
import Language.GraphQL.Error
import Language.GraphQL.Execute.Coerce
import qualified Language.GraphQL.Execute.Transform as Transform
import Language.GraphQL.Trans
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema
import Prelude hiding (null)

resolveFieldValue :: Monad m
    => Type.Value
    -> Type.Subs
    -> ResolverT m a
    -> m (Either Text a)
resolveFieldValue result args =
    flip runReaderT (Context {arguments = Type.Arguments args, values = result})
    . runExceptT
    . runResolverT

collectFields :: Monad m
    => Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> Map Name (NonEmpty (Transform.Field m))
collectFields objectType = foldl forEach Map.empty
  where
    forEach groupedFields (Transform.SelectionField field) =
        let responseKey = aliasOrName field
         in Map.insertWith (<>) responseKey (field :| []) groupedFields
    forEach groupedFields (Transform.SelectionFragment selectionFragment)
        | Transform.Fragment fragmentType fragmentSelectionSet <- selectionFragment
        , doesFragmentTypeApply fragmentType objectType =
            let fragmentGroupedFieldSet = collectFields objectType fragmentSelectionSet
             in Map.unionWith (<>) groupedFields fragmentGroupedFieldSet
        | otherwise = groupedFields

aliasOrName :: forall m. Transform.Field m -> Name
aliasOrName (Transform.Field alias name _ _) = fromMaybe name alias

resolveAbstractType :: Monad m
    => AbstractType m
    -> Type.Subs
    -> CollectErrsT m (Maybe (Out.ObjectType m))
resolveAbstractType abstractType values'
    | Just (Type.String typeName) <- HashMap.lookup "__typename" values' = do
        types' <- gets types
        case HashMap.lookup typeName types' of
            Just (ObjectType objectType) ->
                if instanceOf objectType abstractType
                    then pure $ Just objectType
                    else pure Nothing
            _ -> pure Nothing
    | otherwise = pure Nothing

doesFragmentTypeApply :: forall m
    . CompositeType m
    -> Out.ObjectType m
    -> Bool
doesFragmentTypeApply (CompositeObjectType fragmentType) objectType =
    fragmentType == objectType
doesFragmentTypeApply (CompositeInterfaceType fragmentType) objectType =
    instanceOf objectType $ AbstractInterfaceType fragmentType
doesFragmentTypeApply (CompositeUnionType fragmentType) objectType =
    instanceOf objectType $ AbstractUnionType fragmentType

instanceOf :: forall m. Out.ObjectType m -> AbstractType m -> Bool
instanceOf objectType (AbstractInterfaceType interfaceType) =
    let Out.ObjectType _ _ interfaces _ = objectType
     in foldr go False interfaces
  where
    go objectInterfaceType@(Out.InterfaceType _ _ interfaces _) acc =
        acc || foldr go (interfaceType == objectInterfaceType) interfaces
instanceOf objectType (AbstractUnionType unionType) =
    let Out.UnionType _ _ members = unionType
     in foldr go False members
  where
    go unionMemberType acc = acc || objectType == unionMemberType

executeField :: (Monad m, Serialize a)
    => Out.Field m
    -> Type.Value
    -> NonEmpty (Transform.Field m)
    -> CollectErrsT m a
executeField fieldDefinition prev fields = do
    let Out.Field _ fieldType argumentDefinitions resolver = fieldDefinition
    let (Transform.Field _ _ arguments' _ :| []) = fields
    case coerceArgumentValues argumentDefinitions arguments' of
        Nothing -> errmsg "Argument coercing failed."
        Just argumentValues -> do
            answer <- lift $ resolveFieldValue prev argumentValues resolver
            case answer of
                Right result -> completeValue fieldType fields result
                Left errorMessage -> errmsg errorMessage

completeValue :: (Monad m, Serialize a)
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
        else errmsg "Value completion failed."
completeValue (Out.ObjectBaseType objectType) fields result =
    executeSelectionSet result objectType $ mergeSelectionSets fields
completeValue (Out.InterfaceBaseType interfaceType) fields result
    | Type.Object objectMap <- result = do
        let abstractType = AbstractInterfaceType interfaceType
        concreteType <- resolveAbstractType abstractType objectMap
        case concreteType of
            Just objectType -> executeSelectionSet result objectType
                $ mergeSelectionSets fields
            Nothing -> errmsg "Value completion failed."
completeValue (Out.UnionBaseType unionType) fields result
    | Type.Object objectMap <- result = do
        let abstractType = AbstractUnionType unionType
        concreteType <- resolveAbstractType abstractType objectMap
        case concreteType of
            Just objectType -> executeSelectionSet result objectType
                $ mergeSelectionSets fields
            Nothing -> errmsg "Value completion failed."
completeValue _ _ _ = errmsg "Value completion failed."

mergeSelectionSets :: Monad m => NonEmpty (Transform.Field m) -> Seq (Transform.Selection m)
mergeSelectionSets = foldr forEach mempty
  where
    forEach (Transform.Field _ _ _ fieldSelectionSet) selectionSet =
        selectionSet <> fieldSelectionSet

errmsg :: (Monad m, Serialize a) => Text -> CollectErrsT m a
errmsg errorMessage = addErrMsg errorMessage >> pure null

coerceResult :: (Monad m, Serialize a)
    => Out.Type m
    -> Output a
    -> CollectErrsT m a
coerceResult outputType result
    | Just serialized <- serialize outputType result = pure serialized
    | otherwise = errmsg "Result coercion failed."

-- | Takes an 'Out.ObjectType' and a list of 'Transform.Selection's and applies
-- each field to each 'Transform.Selection'. Resolves into a value containing
-- the resolved 'Transform.Selection', or a null value and error information.
executeSelectionSet :: (Monad m, Serialize a)
    => Type.Value
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> CollectErrsT m a
executeSelectionSet result objectType@(Out.ObjectType _ _ _ resolvers) selectionSet = do
    let fields = collectFields objectType selectionSet
    resolvedValues <- Map.traverseMaybeWithKey forEach fields
    coerceResult (Out.NonNullObjectType objectType) $ Object resolvedValues
  where
    forEach _ fields@(field :| _) =
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
