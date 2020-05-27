{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a representation of a @GraphQL@ Schema in addition to
-- functions for defining and manipulating schemas.
module Language.GraphQL.Schema
    ( Resolver(..)
    , resolve
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST.Document (Name)
import Language.GraphQL.Error
import Language.GraphQL.Execute.Execution
import Language.GraphQL.Execute.Transform
import Language.GraphQL.Trans
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.Out as Out

-- | Resolves a 'Field' into an @Aeson.@'Data.Aeson.Types.Object' with error
-- information (if an error has occurred). @m@ is an arbitrary monad, usually
-- 'IO'.
--
-- Resolving a field can result in a leaf value or an object, which is
-- represented as a list of nested resolvers, used to resolve the fields of that
-- object.
data Resolver m = Resolver Name (ActionT m Value)

resolveFieldValue :: Monad m => Value -> Field m -> ActionT m a -> m (Either Text a)
resolveFieldValue result (Field _ _ args _) =
    flip runReaderT (Context {arguments=args, values=result})
    . runExceptT
    . runActionT

executeField :: Monad m
    => Value
    -> Out.Field m
    -> Field m
    -> CollectErrsT m Aeson.Value
executeField prev (Out.Field _ fieldType _ resolver) field = do
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
    resolve result objectType seqSelection
completeValue (Out.ListBaseType listType) selectionField (List list) =
    Aeson.toJSON <$> traverse (completeValue listType selectionField) list
completeValue _ _ _ = errmsg "Value completion failed."

errmsg :: Monad m => Text -> CollectErrsT m Aeson.Value
errmsg errorMessage = addErrMsg errorMessage >> pure Aeson.Null

-- | Takes a list of 'Resolver's and a list of 'Field's and applies each
-- 'Resolver' to each 'Field'. Resolves into a value containing the
-- resolved 'Field', or a null value and error information.
resolve :: Monad m -- executeSelectionSet
    => Value
    -> Out.ObjectType m
    -> Seq (Selection m)
    -> CollectErrsT m Aeson.Value
resolve result objectType@(Out.ObjectType _ _ _ resolvers) selectionSet = do
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
    {-tryResolvers (Out.SelectionFragment (Out.Fragment typeCondition selections'))
        | Just (Out.Field _ _ _ resolver) <- lookupResolver "__typename" = do
            let fakeField = Out.Field Nothing "__typename" mempty mempty
            that <- lift $ resolveFieldValue result fakeField resolver
            case that of
                Right (String typeCondition')
                    | (Out.CompositeObjectType (Out.ObjectType n _ _ _)) <- typeCondition
                    , typeCondition' == n ->
                        fmap fold . traverse tryResolvers $ selections'
                _ -> pure mempty
        | otherwise = fmap fold . traverse tryResolvers $ selections'-}
