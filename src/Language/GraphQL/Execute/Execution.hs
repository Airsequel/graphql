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
import Data.Map.Strict (Map)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Sequence as Seq
import Language.GraphQL.AST.Document (Name)
import Language.GraphQL.Error
import Language.GraphQL.Execute.Transform
import Language.GraphQL.Trans
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema

resolveFieldValue :: Monad m => Value -> Field m -> ActionT m a -> m (Either Text a)
resolveFieldValue result (Field _ _ args _) =
    flip runReaderT (Context {arguments=args, values=result})
    . runExceptT
    . runActionT

collectFields :: Monad m
    => Out.ObjectType m
    -> Seq (Selection m)
    -> Map Name (Seq (Field m))
collectFields objectType = foldl forEach Map.empty
  where
    forEach groupedFields (SelectionField field) =
        let responseKey = aliasOrName field
         in Map.insertWith (<>) responseKey (Seq.singleton field) groupedFields
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
    -> HashMap Name Value
    -> CollectErrsT m (Maybe (Out.ObjectType m))
resolveAbstractType abstractType values'
    | Just (String typeName) <- HashMap.lookup "__typename" values' = do
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
    let Out.ObjectType fragmentName _ _ _ = fragmentType
        Out.ObjectType objectName _ _ _ = objectType
     in fragmentName == objectName
doesFragmentTypeApply (CompositeInterfaceType fragmentType) objectType =
    instanceOf objectType $ AbstractInterfaceType fragmentType
doesFragmentTypeApply (CompositeUnionType fragmentType) objectType =
    instanceOf objectType $ AbstractUnionType fragmentType

instanceOf :: forall m. Out.ObjectType m -> AbstractType m -> Bool
instanceOf objectType (AbstractInterfaceType interfaceType) =
    let Out.ObjectType _ _ interfaces _ = objectType
     in foldr go False interfaces
  where
    go (Out.InterfaceType that _ interfaces _) acc =
        let Out.InterfaceType this _ _ _ = interfaceType
         in acc || foldr go (this == that) interfaces
instanceOf objectType (AbstractUnionType unionType) =
    let Out.UnionType _ _ members = unionType
     in foldr go False members
  where
    go (Out.ObjectType that _ _ _) acc =
        let Out.ObjectType this _ _ _ = objectType
         in acc || this == that

executeField :: Monad m
    => Value
    -> Out.Resolver m
    -> Field m
    -> CollectErrsT m Aeson.Value
executeField prev (Out.Resolver fieldDefinition resolver) field = do
    let Out.Field _ fieldType _ = fieldDefinition
    answer <- lift $ resolveFieldValue prev field resolver
    case answer of
        Right result -> completeValue fieldType field result
        Left errorMessage -> errmsg errorMessage

completeValue :: Monad m
    => Out.Type m
    -> Field m
    -> Value
    -> CollectErrsT m Aeson.Value
completeValue _ _ Null = pure Aeson.Null
completeValue _ _ (Int integer) = pure $ Aeson.toJSON integer
completeValue _ _ (Boolean boolean') = pure $ Aeson.Bool boolean'
completeValue _ _ (Float float') = pure $ Aeson.toJSON float'
completeValue _ _ (Enum enum) = pure $ Aeson.String enum
completeValue _ _ (String string') = pure $ Aeson.String string'
completeValue (Out.ObjectBaseType objectType) (Field _ _ _ seqSelection) result =
    executeSelectionSet result objectType seqSelection
completeValue (Out.ListBaseType listType) selectionField (List list) =
    Aeson.toJSON <$> traverse (completeValue listType selectionField) list
completeValue (Out.InterfaceBaseType interfaceType) (Field _ _ _ seqSelection) result
    | Object objectMap <- result = do
        abstractType <- resolveAbstractType (AbstractInterfaceType interfaceType) objectMap
        case abstractType of
            Just objectType -> executeSelectionSet result objectType seqSelection
            Nothing -> errmsg "Value completion failed."
completeValue (Out.UnionBaseType unionType) (Field _ _ _ seqSelection) result
    | Object objectMap <- result = do
        abstractType <- resolveAbstractType (AbstractUnionType unionType) objectMap
        case abstractType of
            Just objectType -> executeSelectionSet result objectType seqSelection
            Nothing -> errmsg "Value completion failed."
completeValue _ _ _ = errmsg "Value completion failed."

errmsg :: Monad m => Text -> CollectErrsT m Aeson.Value
errmsg errorMessage = addErrMsg errorMessage >> pure Aeson.Null

-- | Takes an 'Out.ObjectType' and a list of 'Selection's and applies each field
-- to each 'Selection'. Resolves into a value containing the resolved
-- 'Selection', or a null value and error information.
executeSelectionSet :: Monad m
    => Value
    -> Out.ObjectType m
    -> Seq (Selection m)
    -> CollectErrsT m Aeson.Value
executeSelectionSet result objectType@(Out.ObjectType _ _ _ resolvers) selectionSet = do
    resolvedValues <- Map.traverseMaybeWithKey forEach
        $ collectFields objectType selectionSet
    pure $ Aeson.toJSON resolvedValues
  where
    forEach _responseKey (field :<| _) =
        tryResolvers field >>= lift . pure . pure
    forEach _ _ = pure Nothing
    lookupResolver = flip HashMap.lookup resolvers
    tryResolvers fld@(Field _ name _ _)
        | Just typeField <- lookupResolver name =
            executeField result typeField fld
        | otherwise = errmsg $ Text.unwords ["field", name, "not resolved."]
