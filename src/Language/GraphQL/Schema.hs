{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a representation of a @GraphQL@ Schema in addition to
-- functions for defining and manipulating schemas.
module Language.GraphQL.Schema
    ( Resolver
    , Subs
    , object
    , objectA
    , scalar
    , scalarA
    , resolve
    , wrappedObject
    , wrappedObjectA
    , wrappedScalar
    , wrappedScalarA
    -- * AST Reexports
    , Field
    , Argument(..)
    , Value(..)
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (find, fold)
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
--   information (if an error has occurred). @m@ is usually expected to be an
--   instance of 'MonadIO'.
data Resolver m = Resolver
    Text -- ^ Name
    (Field -> CollectErrsT m Aeson.Object) -- ^ Resolver

-- | Variable substitution function.
type Subs = Name -> Maybe Value

-- | Create a new 'Resolver' with the given 'Name' from the given 'Resolver's.
object :: MonadIO m => Name -> ActionT m [Resolver m] -> Resolver m
object name = objectA name . const

-- | Like 'object' but also taking 'Argument's.
objectA :: MonadIO m
    => Name -> ([Argument] -> ActionT m [Resolver m]) -> Resolver m
objectA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ flds) resolver = withField (resolve resolver flds) fld

-- | Like 'object' but also taking 'Argument's and can be null or a list of objects.
wrappedObjectA :: MonadIO m
    => Name -> ([Argument] -> ActionT m (Type.Wrapping [Resolver m])) -> Resolver m
wrappedObjectA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ sels) resolver
        = withField (traverse (`resolve` sels) resolver) fld

-- | Like 'object' but can be null or a list of objects.
wrappedObject :: MonadIO m
    => Name -> ActionT m (Type.Wrapping [Resolver m]) -> Resolver m
wrappedObject name = wrappedObjectA name . const

-- | A scalar represents a primitive value, like a string or an integer.
scalar :: (MonadIO m, Aeson.ToJSON a) => Name -> ActionT m a -> Resolver m
scalar name = scalarA name . const

-- | Like 'scalar' but also taking 'Argument's.
scalarA :: (MonadIO m, Aeson.ToJSON a)
    => Name -> ([Argument] -> ActionT m a) -> Resolver m
scalarA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld result = withField (return result) fld

-- | Like 'scalar' but also taking 'Argument's and can be null or a list of scalars.
wrappedScalarA :: (MonadIO m, Aeson.ToJSON a)
    => Name -> ([Argument] -> ActionT m (Type.Wrapping a)) -> Resolver m
wrappedScalarA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld (Type.Named result) = withField (return result) fld
    resolveRight fld Type.Null
        = return $ HashMap.singleton (aliasOrName fld) Aeson.Null
    resolveRight fld (Type.List result) = withField (return result) fld

-- | Like 'scalar' but can be null or a list of scalars.
wrappedScalar :: (MonadIO m, Aeson.ToJSON a)
    => Name -> ActionT m (Type.Wrapping a) -> Resolver m
wrappedScalar name = wrappedScalarA name . const

resolveFieldValue :: MonadIO m
    => ([Argument] -> ActionT m a)
    -> (Field -> a -> CollectErrsT m (HashMap Text Aeson.Value))
    -> Field
    -> CollectErrsT m (HashMap Text Aeson.Value)
resolveFieldValue f resolveRight fld@(Field _ _ args _) = do
    result <- lift $ reader . runExceptT . runActionT $ f args
    either resolveLeft (resolveRight fld) result
      where
        reader = flip runReaderT $ Context mempty
        resolveLeft err = do
            _ <- addErrMsg err
            return $ HashMap.singleton (aliasOrName fld) Aeson.Null

-- | Helper function to facilitate 'Argument' handling.
withField :: (MonadIO m, Aeson.ToJSON a)
    => CollectErrsT m a -> Field -> CollectErrsT m (HashMap Text Aeson.Value)
withField v fld
    = HashMap.singleton (aliasOrName fld) . Aeson.toJSON <$> runAppendErrs v

-- | Takes a list of 'Resolver's and a list of 'Field's and applies each
--   'Resolver' to each 'Field'. Resolves into a value containing the
--   resolved 'Field', or a null value and error information.
resolve :: MonadIO m
    => [Resolver m] -> Seq Selection -> CollectErrsT m Aeson.Value
resolve resolvers = fmap (Aeson.toJSON . fold) . traverse tryResolvers
  where
    resolveTypeName (Resolver "__typename" f) = do
        value <- f $ Field Nothing "__typename" mempty mempty
        return $ HashMap.lookupDefault "" "__typename" value
    resolveTypeName _ = return ""
    tryResolvers (SelectionField fld@(Field _ name _ _))
        = maybe (errmsg fld) (tryResolver fld) $ find (compareResolvers name) resolvers
    tryResolvers (SelectionFragment (Fragment typeCondition selections')) = do
        that <-  maybe (return "") resolveTypeName (find (compareResolvers "__typename") resolvers)
        if Aeson.String typeCondition == that
            then fmap fold . traverse tryResolvers $ selections'
            else return mempty
    compareResolvers name (Resolver name' _) = name == name'
    tryResolver fld (Resolver _ resolver)  = resolver fld
    errmsg fld@(Field _ name _ _) = do
        addErrMsg $ T.unwords ["field", name, "not resolved."]
        return $ HashMap.singleton (aliasOrName fld) Aeson.Null

aliasOrName :: Field -> Text
aliasOrName (Field alias name _ _) = fromMaybe name alias
