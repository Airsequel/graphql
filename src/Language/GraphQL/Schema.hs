{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a representation of a @GraphQL@ Schema in addition to
-- functions for defining and manipulating schemas.
module Language.GraphQL.Schema
    ( Resolver(..)
    , Subs
    , object
    , resolve
    , resolversToMap
    , scalar
    , wrappedObject
    , wrappedScalar
    -- * AST Reexports
    , Field
    , Value(..)
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
import qualified Language.GraphQL.Type as Type

-- | Resolves a 'Field' into an @Aeson.@'Data.Aeson.Types.Object' with error
--   information (if an error has occurred). @m@ is an arbitrary monad, usually
--   'IO'.
data Resolver m = Resolver
    Text -- ^ Name
    (Field -> CollectErrsT m Aeson.Object) -- ^ Resolver

-- | Converts resolvers to a map.
resolversToMap
    :: (Foldable f, Functor f)
    => f (Resolver m)
    -> HashMap Text (Field -> CollectErrsT m Aeson.Object)
resolversToMap = HashMap.fromList . toList . fmap toKV
  where
    toKV (Resolver name f) = (name, f)

-- | Contains variables for the query. The key of the map is a variable name,
--   and the value is the variable value.
type Subs = HashMap Name Value

-- | Create a new 'Resolver' with the given 'Name' from the given 'Resolver's.
object :: Monad m => Name -> ActionT m [Resolver m] -> Resolver m
object name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ flds) resolver
        = withField (resolve (resolversToMap resolver) flds) fld

-- | Like 'object' but can be null or a list of objects.
wrappedObject ::
    Monad m =>
    Name ->
    ActionT m (Type.Wrapping [Resolver m]) ->
    Resolver m
wrappedObject name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ sels) resolver
        = withField (traverse (resolveMap sels) resolver) fld
    resolveMap = flip (resolve . resolversToMap)

-- | A scalar represents a primitive value, like a string or an integer.
scalar :: (Monad m, Aeson.ToJSON a) => Name -> ActionT m a -> Resolver m
scalar name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld result = withField (return result) fld

-- | Like 'scalar' but can be null or a list of scalars.
wrappedScalar ::
    (Monad m, Aeson.ToJSON a) =>
    Name ->
    ActionT m (Type.Wrapping a) ->
    Resolver m
wrappedScalar name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld (Type.Named result) = withField (return result) fld
    resolveRight fld Type.Null
        = return $ HashMap.singleton (aliasOrName fld) Aeson.Null
    resolveRight fld (Type.List result) = withField (return result) fld

resolveFieldValue ::
    Monad m =>
    ActionT m a ->
    (Field -> a -> CollectErrsT m Aeson.Object) ->
    Field ->
    CollectErrsT m (HashMap Text Aeson.Value)
resolveFieldValue f resolveRight fld@(Field _ _ args _) = do
    result <- lift $ reader . runExceptT . runActionT $ f
    either resolveLeft (resolveRight fld) result
      where
        reader = flip runReaderT $ Context {arguments=args}
        resolveLeft err = do
            _ <- addErrMsg err
            return $ HashMap.singleton (aliasOrName fld) Aeson.Null

-- | Helper function to facilitate error handling and result emitting.
withField :: (Monad m, Aeson.ToJSON a)
    => CollectErrsT m a -> Field -> CollectErrsT m (HashMap Text Aeson.Value)
withField v fld
    = HashMap.singleton (aliasOrName fld) . Aeson.toJSON <$> runAppendErrs v

-- | Takes a list of 'Resolver's and a list of 'Field's and applies each
--   'Resolver' to each 'Field'. Resolves into a value containing the
--   resolved 'Field', or a null value and error information.
resolve :: Monad m
    => HashMap Text (Field -> CollectErrsT m Aeson.Object)
    -> Seq Selection
    -> CollectErrsT m Aeson.Value
resolve resolvers = fmap (Aeson.toJSON . fold) . traverse tryResolvers
  where
    resolveTypeName f = do
        value <- f $ Field Nothing "__typename" mempty mempty
        return $ HashMap.lookupDefault "" "__typename" value
    tryResolvers (SelectionField fld@(Field _ name _ _))
        = fromMaybe (errmsg fld) $ HashMap.lookup name resolvers <*> Just fld
    tryResolvers (SelectionFragment (Fragment typeCondition selections')) = do
        that <- traverse resolveTypeName $ HashMap.lookup "__typename" resolvers
        if maybe True (Aeson.String typeCondition ==) that
            then fmap fold . traverse tryResolvers $ selections'
            else return mempty
    errmsg fld@(Field _ name _ _) = do
        addErrMsg $ T.unwords ["field", name, "not resolved."]
        return $ HashMap.singleton (aliasOrName fld) Aeson.Null

aliasOrName :: Field -> Text
aliasOrName (Field alias name _ _) = fromMaybe name alias
