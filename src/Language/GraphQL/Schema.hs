{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a representation of a @GraphQL@ Schema in addition to
-- functions for defining and manipulating schemas.
module Language.GraphQL.Schema
    ( Resolver(..)
    , Subs
    , object
    , resolve
    , resolversToMap
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (fold, toList)
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Language.GraphQL.AST.Core
import Language.GraphQL.Error
import Language.GraphQL.Trans
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out

-- | Resolves a 'Field' into an @Aeson.@'Data.Aeson.Types.Object' with error
-- information (if an error has occurred). @m@ is an arbitrary monad, usually
-- 'IO'.
--
-- Resolving a field can result in a leaf value or an object, which is
-- represented as a list of nested resolvers, used to resolve the fields of that
-- object.
data Resolver m = Resolver Name (ActionT m (Out.Value m))

-- | Converts resolvers to a map.
resolversToMap :: (Foldable f, Functor f)
    => forall m
    . f (Resolver m)
    -> HashMap Text (ActionT m (Out.Value m))
resolversToMap = HashMap.fromList . toList . fmap toKV
  where
    toKV (Resolver name r) = (name, r)

-- | Contains variables for the query. The key of the map is a variable name,
--   and the value is the variable value.
type Subs = HashMap Name In.Value

-- | Create a new 'Resolver' with the given 'Name' from the given
-- Resolver's.
object :: Monad m => [Resolver m] -> Out.Value m
object = Out.Object . resolversToMap

resolveFieldValue :: Monad m => Field -> ActionT m a -> m (Either Text a)
resolveFieldValue field@(Field _ _ args _) =
    flip runReaderT (Context {arguments=args, info=field})
    . runExceptT
    . runActionT

withField :: Monad m
    => Field
    -> ActionT m (Out.Value m)
    -> CollectErrsT m Aeson.Object
withField field resolver = do
    answer <- lift $ resolveFieldValue field resolver
    case answer of
        Right result -> HashMap.singleton (aliasOrName field)
            <$> toJSON field result
        Left errorMessage -> errmsg field errorMessage

toJSON :: Monad m => Field -> Out.Value m -> CollectErrsT m Aeson.Value
toJSON _ Out.Null = pure Aeson.Null
toJSON _ (Out.Int integer) = pure $ Aeson.toJSON integer
toJSON _ (Out.Boolean boolean) = pure $ Aeson.Bool boolean
toJSON _ (Out.Float float) = pure $ Aeson.toJSON float
toJSON _ (Out.Enum enum) = pure $ Aeson.String enum
toJSON _ (Out.String string) = pure $ Aeson.String string
toJSON field (Out.List list) = Aeson.toJSON <$> traverse (toJSON field) list
toJSON (Field _ _ _ seqSelection) (Out.Object map') =
    map' `resolve` seqSelection

errmsg :: Monad m => Field -> Text -> CollectErrsT m (HashMap Text Aeson.Value)
errmsg field errorMessage = do
    addErrMsg errorMessage
    pure $ HashMap.singleton (aliasOrName field) Aeson.Null

-- | Takes a list of 'Resolver's and a list of 'Field's and applies each
-- 'Resolver' to each 'Field'. Resolves into a value containing the
-- resolved 'Field', or a null value and error information.
resolve :: Monad m
    => HashMap Text (ActionT m (Out.Value m))
    -> Seq Selection
    -> CollectErrsT m Aeson.Value
resolve resolvers = fmap (Aeson.toJSON . fold) . traverse tryResolvers
  where
    lookupResolver = flip HashMap.lookup resolvers
    tryResolvers (SelectionField fld@(Field _ name _ _))
        | (Just resolver) <- lookupResolver name = withField fld resolver
        | otherwise = errmsg fld $ T.unwords ["field", name, "not resolved."]
    tryResolvers (SelectionFragment (Fragment typeCondition selections'))
        | Just resolver <- lookupResolver "__typename" = do
            let fakeField = Field Nothing "__typename" mempty mempty
            that <- lift $ resolveFieldValue fakeField resolver
            case that of
                Right (Out.String typeCondition')
                    | typeCondition' == typeCondition ->
                        fmap fold . traverse tryResolvers $ selections'
                _ -> pure mempty
        | otherwise = fmap fold . traverse tryResolvers $ selections'

aliasOrName :: Field -> Text
aliasOrName (Field alias name _ _) = fromMaybe name alias
