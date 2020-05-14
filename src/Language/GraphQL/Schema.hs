{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a representation of a @GraphQL@ Schema in addition to
-- functions for defining and manipulating schemas.
module Language.GraphQL.Schema
    ( FieldResolver(..)
    , Resolver(..)
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
data Resolver m = Resolver Name (FieldResolver m)

data FieldResolver m
    = ValueResolver (ActionT m Aeson.Value)
    | NestingResolver (ActionT m (Type.Wrapping (HashMap Name (FieldResolver m))))

-- | Converts resolvers to a map.
resolversToMap :: (Foldable f, Functor f)
    => f (Resolver m)
    -> HashMap Text (FieldResolver m)
resolversToMap = HashMap.fromList . toList . fmap toKV
  where
    toKV (Resolver name r) = (name, r)

-- | Contains variables for the query. The key of the map is a variable name,
--   and the value is the variable value.
type Subs = HashMap Name Value

-- | Create a new 'Resolver' with the given 'Name' from the given 'Resolver's.
object :: Monad m => Name -> ActionT m [Resolver m] -> Resolver m
object name = Resolver name
    . NestingResolver
    . fmap (Type.Named . resolversToMap)

-- | Like 'object' but can be null or a list of objects.
wrappedObject :: Monad m
    => Name
    -> ActionT m (Type.Wrapping [Resolver m])
    -> Resolver m
wrappedObject name = Resolver name
    . NestingResolver
    . (fmap . fmap) resolversToMap

-- | A scalar represents a primitive value, like a string or an integer.
scalar :: (Monad m, Aeson.ToJSON a) => Name -> ActionT m a -> Resolver m
scalar name = Resolver name . ValueResolver . fmap Aeson.toJSON

-- | Like 'scalar' but can be null or a list of scalars.
wrappedScalar :: (Monad m, Aeson.ToJSON a)
    => Name
    -> ActionT m (Type.Wrapping a)
    -> Resolver m
wrappedScalar name = Resolver name . ValueResolver . fmap Aeson.toJSON

resolveFieldValue :: Monad m => Field -> ActionT m a -> m (Either Text a)
resolveFieldValue field@(Field _ _ args _) =
    flip runReaderT (Context {arguments=args, info=field})
    . runExceptT
    . runActionT

convert :: Type.Wrapping Aeson.Value -> Aeson.Value
convert Type.Null = Aeson.Null
convert (Type.Named value) = value
convert (Type.List value) = Aeson.toJSON value

withField :: Monad m => Field -> FieldResolver m -> CollectErrsT m Aeson.Object
withField field (ValueResolver resolver) = do
    answer <- lift $ resolveFieldValue field resolver
    either (errmsg field) (pure . HashMap.singleton (aliasOrName field)) answer
withField field@(Field _ _ _ seqSelection) (NestingResolver resolver) = do
    answer <- lift $ resolveFieldValue field resolver
    case answer of
        Right result -> do
            nestedFields <- traverse (`resolve` seqSelection) result
            pure $ HashMap.singleton (aliasOrName field) $ convert nestedFields
        Left errorMessage -> errmsg field errorMessage

errmsg :: Monad m => Field -> Text -> CollectErrsT m (HashMap Text Aeson.Value)
errmsg field errorMessage = do
    addErrMsg errorMessage
    pure $ HashMap.singleton (aliasOrName field) Aeson.Null

-- | Takes a list of 'Resolver's and a list of 'Field's and applies each
--   'Resolver' to each 'Field'. Resolves into a value containing the
--   resolved 'Field', or a null value and error information.
resolve :: Monad m
    => HashMap Text (FieldResolver m)
    -> Seq Selection
    -> CollectErrsT m Aeson.Value
resolve resolvers = fmap (Aeson.toJSON . fold) . traverse tryResolvers
  where
    lookupResolver = flip HashMap.lookup resolvers
    tryResolvers (SelectionField fld@(Field _ name _ _))
        | (Just resolver) <- lookupResolver name = withField fld resolver
        | otherwise = errmsg fld $ T.unwords ["field", name, "not resolved."]
    tryResolvers (SelectionFragment (Fragment typeCondition selections'))
        | Just (ValueResolver resolver) <- lookupResolver "__typename" = do
            let fakeField = Field Nothing "__typename" mempty mempty
            that <- lift $ resolveFieldValue fakeField resolver
            if Right (Aeson.String typeCondition) == that
                then fmap fold . traverse tryResolvers $ selections'
                else pure mempty
        | otherwise = fmap fold . traverse tryResolvers $ selections'

aliasOrName :: Field -> Text
aliasOrName (Field alias name _ _) = fromMaybe name alias
