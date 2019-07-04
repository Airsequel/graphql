{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a representation of a @GraphQL@ Schema in addition to
--   functions for defining and manipulating Schemas.
module Data.GraphQL.Schema
  ( Schema
  , Resolver
  , Subs
  , nullableArray
  , nullableEnum
  , nullableObject
  , nullableScalar
  , object
  , objectA
  , scalar
  , scalarA
  , array
  , arrayA
  , enum
  , enumA
  , resolve
  -- * AST Reexports
  , Field
  , Argument(..)
  , Value(..)
  ) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Data.Foldable ( find
                     , fold
                     )
import Data.GraphQL.Error
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T
import Language.GraphQL.Trans
import Data.GraphQL.AST.Core

-- | A GraphQL schema.
--   @f@ is usually expected to be an instance of 'Alternative'.
type Schema m = NonEmpty (Resolver m)

-- | Resolves a 'Field' into an @Aeson.@'Aeson.Object' with error information
--   (or 'empty'). @m@ is usually expected to be an instance of 'MonadPlus'.
data Resolver m = Resolver
    Text -- ^ Name
    (Field -> CollectErrsT m Aeson.Object) -- ^ Resolver

type Fields = [Field]

type Arguments = [Argument]

-- | Variable substitution function.
type Subs = Name -> Maybe Value

-- | Create a new 'Resolver' with the given 'Name' from the given 'Resolver's.
object :: MonadPlus m => Name -> ActionT m [Resolver m] -> Resolver m
object name = objectA name . const

-- | Like 'object' but also taking 'Argument's.
objectA :: MonadPlus m
    => Name -> (Arguments -> ActionT m [Resolver m]) -> Resolver m
objectA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ flds) resolver = withField (resolve resolver flds) fld

-- | Like 'object' but can be null.
nullableObject :: MonadPlus m
    => Name -> (Arguments -> ActionT m (Maybe [Resolver m])) -> Resolver m
nullableObject name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ flds) (Just resolver) = withField (resolve resolver flds) fld
    resolveRight fld Nothing
        = return $ HashMap.singleton (aliasOrName fld) Aeson.Null

-- | A scalar represents a primitive value, like a string or an integer.
scalar :: (MonadPlus m, Aeson.ToJSON a) => Name -> ActionT m a -> Resolver m
scalar name = scalarA name . const

-- | Like 'scalar' but also taking 'Argument's.
scalarA :: (MonadPlus m, Aeson.ToJSON a)
    => Name -> (Arguments -> ActionT m a) -> Resolver m
scalarA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ []) result = withField (return result) fld
    resolveRight _ _ = mzero

-- | Lika 'scalar' but can be null.
nullableScalar :: (MonadPlus m, Aeson.ToJSON a)
    => Name -> (Arguments -> ActionT m (Maybe a)) -> Resolver m
nullableScalar name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ []) (Just result) = withField (return result) fld
    resolveRight fld Nothing
        = return $ HashMap.singleton (aliasOrName fld) Aeson.Null
    resolveRight _ _ = mzero

-- | Creates a list of 'Resolver's.
array :: MonadPlus m => Name -> ActionT m [[Resolver m]] -> Resolver m
array name = arrayA name . const

-- | Like 'array' but also taking 'Argument's.
arrayA :: MonadPlus m
    => Name -> (Arguments -> ActionT m [[Resolver m]]) -> Resolver m
arrayA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ sels) resolver
        = withField (traverse (`resolve` sels) resolver) fld

-- | Like 'array' but can be null.
nullableArray :: MonadPlus m
    => Name -> (Arguments -> ActionT m (Maybe [[Resolver m]])) -> Resolver m
nullableArray name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ sels) (Just resolver)
        = withField (traverse (`resolve` sels) resolver) fld
    resolveRight fld Nothing
        = return $ HashMap.singleton (aliasOrName fld) Aeson.Null

-- | Represents one of a finite set of possible values.
--   Used in place of a 'scalar' when the possible responses are easily enumerable.
enum :: MonadPlus m => Name -> ActionT m [Text] -> Resolver m
enum name = enumA name . const

-- | Like 'enum' but also taking 'Argument's.
enumA :: MonadPlus m => Name -> (Arguments -> ActionT m [Text]) -> Resolver m
enumA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld resolver = withField (return resolver) fld

-- | Like 'array'' but also taking 'Argument's.
nullableEnum :: MonadPlus m
    => Name -> (Arguments -> ActionT m (Maybe [Text])) -> Resolver m
nullableEnum name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld (Just resolver) = withField (return resolver) fld
    resolveRight fld Nothing
        = return $ HashMap.singleton (aliasOrName fld) Aeson.Null

resolveFieldValue :: MonadPlus m
    => ([Argument] -> ActionT m a)
    -> (Field -> a -> CollectErrsT m (HashMap Text Aeson.Value))
    -> Field
    -> CollectErrsT m (HashMap Text Aeson.Value)
resolveFieldValue f resolveRight fld@(Field _ _ args _) = do
    result <- lift $ runExceptT . runActionT $ f args
    either resolveLeft (resolveRight fld) result
      where
        resolveLeft err = do
            _ <- addErrMsg err
            return $ HashMap.singleton (aliasOrName fld) Aeson.Null

-- | Helper function to facilitate 'Argument' handling.
withField :: (MonadPlus m, Aeson.ToJSON a)
    => CollectErrsT m a -> Field -> CollectErrsT m (HashMap Text Aeson.Value)
withField v fld
    = HashMap.singleton (aliasOrName fld) . Aeson.toJSON <$> runAppendErrs v

-- | Takes a list of 'Resolver's and a list of 'Field's and applies each
--   'Resolver' to each 'Field'. Resolves into a value containing the
--   resolved 'Field', or a null value and error information.
resolve :: MonadPlus m
    => [Resolver m] -> Fields -> CollectErrsT m Aeson.Value
resolve resolvers = fmap (Aeson.toJSON . fold) . traverse tryResolvers
  where
    tryResolvers fld = mplus (maybe mzero (tryResolver fld) $ find (compareResolvers fld) resolvers) $ errmsg fld
    compareResolvers (Field _ name _ _) (Resolver name' _) = name == name'
    tryResolver fld (Resolver _ resolver)  = resolver fld
    errmsg fld@(Field _ name _ _) = do
        addErrMsg $ T.unwords ["field", name, "not resolved."]
        return $ HashMap.singleton (aliasOrName fld) Aeson.Null

aliasOrName :: Field -> Text
aliasOrName (Field alias name _ _) = fromMaybe name alias
