{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a representation of a @GraphQL@ Schema in addition to
--   functions for defining and manipulating Schemas.
module Data.GraphQL.Schema
  ( Schema
  , Resolver
  , Subs
  , object
  , object'
  , objectA
  , objectA'
  , scalar
  , scalarA
  , array
  , array'
  , arrayA
  , arrayA'
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
    resolveRight fld@(Field _ _ _ flds) resolver = withField name (resolve resolver flds) fld

-- | Create a named 'Resolver' from a list of 'Resolver's.
object' :: MonadPlus m => Name -> ActionT m [Resolver m] -> Resolver m
object' name = objectA' name . const

-- | Like 'object'' but also taking 'Argument's.
objectA' :: MonadPlus m
    => Name -> (Arguments -> ActionT m [Resolver m]) -> Resolver m
objectA' name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ flds) resolver = withField name (resolve resolver flds) fld

-- | A scalar represents a primitive value, like a string or an integer.
scalar :: (MonadPlus m, Aeson.ToJSON a) => Name -> ActionT m a -> Resolver m
scalar name = scalarA name . const

-- | Like 'scalar' but also taking 'Argument's.
scalarA :: (MonadPlus m, Aeson.ToJSON a)
    => Name -> (Arguments -> ActionT m a) -> Resolver m
scalarA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ []) result = withField name (return result) fld
    resolveRight _ _ = mzero

array :: MonadPlus m => Name -> ActionT m [[Resolver m]] -> Resolver m
array name = arrayA name . const

-- | Like 'array' but also taking 'Argument's.
arrayA :: MonadPlus m
    => Name -> (Arguments -> ActionT m [[Resolver m]]) -> Resolver m
arrayA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ sels) resolver
        = withField name (traverse (`resolve` sels) resolver) fld

-- | Like 'object'' but taking lists of 'Resolver's instead of a single list.
array' :: MonadPlus m => Name -> ActionT m [[Resolver m]] -> Resolver m
array' name = arrayA' name . const

-- | Like 'array'' but also taking 'Argument's.
arrayA' :: MonadPlus m
    => Name -> (Arguments -> ActionT m [[Resolver m]]) -> Resolver m
arrayA' name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld@(Field _ _ _ sels) resolver
        = withField name (traverse (`resolve` sels) resolver) fld

-- | Represents one of a finite set of possible values.
--   Used in place of a 'scalar' when the possible responses are easily enumerable.
enum :: MonadPlus m => Name -> ActionT m [Text] -> Resolver m
enum name = enumA name . const

-- | Like 'enum' but also taking 'Argument's.
enumA :: MonadPlus m => Name -> (Arguments -> ActionT m [Text]) -> Resolver m
enumA name f = Resolver name $ resolveFieldValue f resolveRight
  where
    resolveRight fld resolver = withField name (return resolver) fld

resolveFieldValue :: MonadPlus m
    => ([Argument] -> ActionT m a)
    -> (Field -> a -> CollectErrsT m (HashMap Text Aeson.Value))
    -> Field
    -> CollectErrsT m (HashMap Text Aeson.Value)
resolveFieldValue f resolveRight fld@(Field alias name args _) = do
    result <- lift $ runExceptT . runActionT $ f args
    either resolveLeft (resolveRight fld) result
      where
        resolveLeft err = do
            _ <- addErrMsg err
            return $ HashMap.singleton (fromMaybe name alias) Aeson.Null

-- | Helper function to facilitate 'Argument' handling.
withField :: (MonadPlus m, Aeson.ToJSON a)
    => Name -> CollectErrsT m a -> Field -> CollectErrsT m (HashMap Text Aeson.Value)
withField name v (Field alias _ _ _)
    = HashMap.singleton aliasOrName . Aeson.toJSON <$> runAppendErrs v
    {- TODO: Report error when Non-Nullable type for field argument.
    else return (HashMap.singleton aliasOrName Aeson.Null) -}
  where
    aliasOrName = fromMaybe name alias

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
    errmsg (Field alias name _ _) = do
        addErrMsg $ T.unwords ["field", name, "not resolved."]
        return $ HashMap.singleton aliasOrName Aeson.Null
          where
            aliasOrName = fromMaybe name alias
