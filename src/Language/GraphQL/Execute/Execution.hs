{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.Execute.Execution
    ( executeSelectionSet
    ) where

import qualified Data.Aeson as Aeson
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
import Language.GraphQL.AST.Core
import Language.GraphQL.Error
import Language.GraphQL.Execute.Coerce
import Language.GraphQL.Execute.Transform
import Language.GraphQL.Trans
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema

resolveFieldValue :: Monad m
    => Definition.Value
    -> Definition.Subs
    -> ActionT m a
    -> m (Either Text a)
resolveFieldValue result args =
    flip runReaderT (Context {arguments = Arguments args, values = result})
    . runExceptT
    . runActionT

collectFields :: Monad m
    => Out.ObjectType m
    -> Seq (Selection m)
    -> Map Name (NonEmpty (Field m))
collectFields objectType = foldl forEach Map.empty
  where
    forEach groupedFields (SelectionField field) =
        let responseKey = aliasOrName field
         in Map.insertWith (<>) responseKey (field :| []) groupedFields
    forEach groupedFields (SelectionFragment selectionFragment)
        | Fragment fragmentType fragmentSelectionSet <- selectionFragment
        , doesFragmentTypeApply fragmentType objectType =
            let fragmentGroupedFieldSet = collectFields objectType fragmentSelectionSet
             in Map.unionWith (<>) groupedFields fragmentGroupedFieldSet
        | otherwise = groupedFields

aliasOrName :: forall m. Field m -> Name
aliasOrName (Field alias name _ _) = fromMaybe name alias

resolveAbstractType :: Monad m
    => AbstractType m
    -> HashMap Name Definition.Value
    -> CollectErrsT m (Maybe (Out.ObjectType m))
resolveAbstractType abstractType values'
    | Just (Definition.String typeName) <- HashMap.lookup "__typename" values' = do
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

executeField :: Monad m
    => Out.Resolver m
    -> Definition.Value
    -> NonEmpty (Field m)
    -> CollectErrsT m Aeson.Value
executeField (Out.Resolver fieldDefinition resolver) prev fields = do
    let Out.Field _ fieldType argumentDefinitions = fieldDefinition
    let (Field _ _ arguments' _ :| []) = fields
    case coerceArgumentValues argumentDefinitions arguments' of
        Nothing -> errmsg "Argument coercing failed."
        Just argumentValues -> do
            answer <- lift $ resolveFieldValue prev argumentValues resolver
            case answer of
                Right result -> completeValue fieldType fields result
                Left errorMessage -> errmsg errorMessage

completeValue :: Monad m
    => Out.Type m
    -> NonEmpty (Field m)
    -> Definition.Value
    -> CollectErrsT m Aeson.Value
completeValue _ _ Definition.Null = pure Aeson.Null
completeValue _ _ (Definition.Int integer) = pure $ Aeson.toJSON integer
completeValue _ _ (Definition.Boolean boolean') = pure $ Aeson.Bool boolean'
completeValue _ _ (Definition.Float float') = pure $ Aeson.toJSON float'
completeValue _ _ (Definition.Enum enum) = pure $ Aeson.String enum
completeValue _ _ (Definition.String string') = pure $ Aeson.String string'
completeValue (Out.ListBaseType listType) fields (Definition.List list) =
    Aeson.toJSON <$> traverse (completeValue listType fields) list
completeValue (Out.ObjectBaseType objectType) fields result =
    executeSelectionSet result objectType $ mergeSelectionSets fields
completeValue (Out.InterfaceBaseType interfaceType) fields result
    | Definition.Object objectMap <- result = do
        abstractType <- resolveAbstractType (AbstractInterfaceType interfaceType) objectMap
        case abstractType of
            Just objectType -> executeSelectionSet result objectType
                $ mergeSelectionSets fields
            Nothing -> errmsg "Value completion failed."
completeValue (Out.UnionBaseType unionType) fields result
    | Definition.Object objectMap <- result = do
        abstractType <- resolveAbstractType (AbstractUnionType unionType) objectMap
        case abstractType of
            Just objectType -> executeSelectionSet result objectType
                $ mergeSelectionSets fields
            Nothing -> errmsg "Value completion failed."
completeValue _ _ _ = errmsg "Value completion failed."

mergeSelectionSets :: Monad m => NonEmpty (Field m) -> Seq (Selection m)
mergeSelectionSets fields = foldr forEach mempty fields
  where
    forEach (Field _ _ _ fieldSelectionSet) selectionSet =
        selectionSet <> fieldSelectionSet

errmsg :: Monad m => Text -> CollectErrsT m Aeson.Value
errmsg errorMessage = addErrMsg errorMessage >> pure Aeson.Null

-- | Takes an 'Out.ObjectType' and a list of 'Selection's and applies each field
-- to each 'Selection'. Resolves into a value containing the resolved
-- 'Selection', or a null value and error information.
executeSelectionSet :: Monad m
    => Definition.Value
    -> Out.ObjectType m
    -> Seq (Selection m)
    -> CollectErrsT m Aeson.Value
executeSelectionSet result objectType@(Out.ObjectType _ _ _ resolvers) selectionSet = do
    let fields = collectFields objectType selectionSet
    resolvedValues <- Map.traverseMaybeWithKey forEach fields
    pure $ Aeson.toJSON resolvedValues
  where
    forEach _ fields@(field :| _) =
        let Field _ name _ _ = field
         in traverse (tryResolver fields) $ lookupResolver name
    lookupResolver = flip HashMap.lookup resolvers
    tryResolver fields resolver =
        executeField resolver result fields >>= lift . pure

coerceArgumentValues
    :: HashMap Name In.Argument
    -> HashMap Name Input
    -> Maybe Definition.Subs
coerceArgumentValues argumentDefinitions argumentValues =
    HashMap.foldrWithKey forEach (pure mempty) argumentDefinitions
  where
    forEach variableName (In.Argument _ variableType defaultValue) =
        matchFieldValues coerceArgumentValue argumentValues variableName variableType defaultValue
    coerceArgumentValue inputType (Int integer) =
        coerceInputLiteral inputType (Definition.Int integer)
    coerceArgumentValue inputType (Boolean boolean) =
        coerceInputLiteral inputType (Definition.Boolean boolean)
    coerceArgumentValue inputType (String string) =
        coerceInputLiteral inputType (Definition.String string)
    coerceArgumentValue inputType (Float float) =
        coerceInputLiteral inputType (Definition.Float float)
    coerceArgumentValue inputType (Enum enum) =
        coerceInputLiteral inputType (Definition.Enum enum)
    coerceArgumentValue inputType Null
        | In.isNonNullType inputType = Nothing
        | otherwise = coerceInputLiteral inputType Definition.Null
    coerceArgumentValue (In.ListBaseType inputType) (List list) =
        let coerceItem = coerceInputLiteral inputType
         in Definition.List <$> traverse coerceItem list
    coerceArgumentValue (In.InputObjectBaseType inputType) (Object object)
        | In.InputObjectType _ _ inputFields <- inputType = 
            let go = forEachField object
                resultMap = HashMap.foldrWithKey go (pure mempty) inputFields
             in Definition.Object <$> resultMap
    coerceArgumentValue _ (Variable variable) = pure variable
    coerceArgumentValue _ _ = Nothing
    forEachField object variableName (In.InputField _ variableType defaultValue) =
        matchFieldValues coerceArgumentValue object variableName variableType defaultValue
