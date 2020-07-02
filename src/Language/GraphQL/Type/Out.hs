{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Output types and values, monad transformer stack used by the @GraphQL@
-- resolvers.
--
-- This module is intended to be imported qualified, to avoid name clashes
-- with 'Language.GraphQL.Type.In'.
module Language.GraphQL.Type.Out
    ( Context(..)
    , Field(..)
    , InterfaceType(..)
    , ObjectType(..)
    , ResolverT(..)
    , Type(..)
    , UnionType(..)
    , argument
    , isNonNullType
    , pattern EnumBaseType
    , pattern InterfaceBaseType
    , pattern ListBaseType
    , pattern ObjectBaseType
    , pattern ScalarBaseType
    , pattern UnionBaseType
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.GraphQL.AST (Name)
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.In as In

-- | Object type definition.
--
-- Almost all of the GraphQL types you define will be object types. Object
-- types have a name, but most importantly describe their fields.
data ObjectType m = ObjectType
    Name (Maybe Text) [InterfaceType m] (HashMap Name (Field m))

instance forall a. Eq (ObjectType a) where
    (ObjectType this _ _ _) == (ObjectType that _ _ _) = this == that

-- | Interface Type Definition.
--
-- When a field can return one of a heterogeneous set of types, a Interface type
-- is used to describe what types are possible, and what fields are in common
-- across all types.
data InterfaceType m = InterfaceType
    Name (Maybe Text) [InterfaceType m] (HashMap Name (Field m))

instance forall a. Eq (InterfaceType a) where
    (InterfaceType this _ _ _) == (InterfaceType that _ _ _) = this == that

-- | Union Type Definition.
--
-- When a field can return one of a heterogeneous set of types, a Union type is
-- used to describe what types are possible.
data UnionType m = UnionType Name (Maybe Text) [ObjectType m]

instance forall a. Eq (UnionType a) where
    (UnionType this _ _) == (UnionType that _ _) = this == that

-- | Output object field definition.
data Field m = Field
    (Maybe Text) -- ^ Description.
    (Type m) -- ^ Field type.
    (HashMap Name In.Argument) -- ^ Arguments.
    (ResolverT m Value) -- ^ Resolver.

-- | These types may be used as output types as the result of fields.
--
-- GraphQL distinguishes between "wrapping" and "named" types. Each wrapping
-- type can wrap other wrapping or named types. Wrapping types are lists and
-- Non-Null types (named types are nullable by default).
data Type m
    = NamedScalarType ScalarType
    | NamedEnumType EnumType
    | NamedObjectType (ObjectType m)
    | NamedInterfaceType (InterfaceType m)
    | NamedUnionType (UnionType m)
    | ListType (Type m)
    | NonNullScalarType ScalarType
    | NonNullEnumType EnumType
    | NonNullObjectType (ObjectType m)
    | NonNullInterfaceType (InterfaceType m)
    | NonNullUnionType (UnionType m)
    | NonNullListType (Type m)
    deriving Eq

-- | Matches either 'NamedScalarType' or 'NonNullScalarType'.
pattern ScalarBaseType :: forall m. ScalarType -> Type m
pattern ScalarBaseType scalarType <- (isScalarType -> Just scalarType)

-- | Matches either 'NamedEnumType' or 'NonNullEnumType'.
pattern EnumBaseType :: forall m. EnumType -> Type m
pattern EnumBaseType enumType <- (isEnumType -> Just enumType)

-- | Matches either 'NamedObjectType' or 'NonNullObjectType'.
pattern ObjectBaseType :: forall m. ObjectType m -> Type m
pattern ObjectBaseType objectType <- (isObjectType -> Just objectType)

-- | Matches either 'NamedInterfaceType' or 'NonNullInterfaceType'.
pattern InterfaceBaseType :: forall m. InterfaceType m -> Type m
pattern InterfaceBaseType interfaceType <-
    (isInterfaceType -> Just interfaceType)

-- | Matches either 'NamedUnionType' or 'NonNullUnionType'.
pattern UnionBaseType :: forall m. UnionType m -> Type m
pattern UnionBaseType unionType <- (isUnionType -> Just unionType)

-- | Matches either 'ListType' or 'NonNullListType'.
pattern ListBaseType :: forall m. Type m -> Type m
pattern ListBaseType listType <- (isListType -> Just listType)

{-# COMPLETE ScalarBaseType
    , EnumBaseType
    , ObjectBaseType
    , ListBaseType
    , InterfaceBaseType
    , UnionBaseType
    #-}

isScalarType :: forall m. Type m -> Maybe ScalarType
isScalarType (NamedScalarType outputType) = Just outputType
isScalarType (NonNullScalarType outputType) = Just outputType
isScalarType _ = Nothing

isObjectType :: forall m. Type m -> Maybe (ObjectType m)
isObjectType (NamedObjectType outputType) = Just outputType
isObjectType (NonNullObjectType outputType) = Just outputType
isObjectType _ = Nothing

isEnumType :: forall m. Type m -> Maybe EnumType
isEnumType (NamedEnumType outputType) = Just outputType
isEnumType (NonNullEnumType outputType) = Just outputType
isEnumType _ = Nothing

isInterfaceType :: forall m. Type m -> Maybe (InterfaceType m)
isInterfaceType (NamedInterfaceType interfaceType) = Just interfaceType
isInterfaceType (NonNullInterfaceType interfaceType) = Just interfaceType
isInterfaceType _ = Nothing

isUnionType :: forall m. Type m -> Maybe (UnionType m)
isUnionType (NamedUnionType unionType) = Just unionType
isUnionType (NonNullUnionType unionType) = Just unionType
isUnionType _ = Nothing

isListType :: forall m. Type m -> Maybe (Type m)
isListType (ListType outputType) = Just outputType
isListType (NonNullListType outputType) = Just outputType
isListType _ = Nothing

-- | Checks whether the given output type is a non-null type.
isNonNullType :: forall m. Type m -> Bool
isNonNullType (NonNullScalarType _) = True
isNonNullType (NonNullEnumType _) = True
isNonNullType (NonNullObjectType _) = True
isNonNullType (NonNullInterfaceType _) = True
isNonNullType (NonNullUnionType _) = True
isNonNullType (NonNullListType _) = True
isNonNullType _ = False

-- | Resolution context holds resolver arguments.
data Context = Context
    { arguments :: Arguments
    , values :: Value
    }

-- | Monad transformer stack used by the resolvers to provide error handling
-- and resolution context (resolver arguments).
--
-- Resolves a 'Field' into a 'Value' with error information (if an error has
-- occurred). @m@ is an arbitrary monad, usually 'IO'.
--
-- Resolving a field can result in a leaf value or an object, which is
-- represented as a list of nested resolvers, used to resolve the fields of that
-- object.
newtype ResolverT m a = ResolverT
    { runResolverT :: ExceptT Text (ReaderT Context m) a
    }

instance Functor m => Functor (ResolverT m) where
    fmap f = ResolverT . fmap f . runResolverT

instance Monad m => Applicative (ResolverT m) where
    pure = ResolverT . pure
    (ResolverT f) <*> (ResolverT x) = ResolverT $ f <*> x

instance Monad m => Monad (ResolverT m) where
    return = pure
    (ResolverT action) >>= f = ResolverT $ action >>= runResolverT . f

instance MonadTrans ResolverT where
    lift = ResolverT . lift . lift

instance MonadIO m => MonadIO (ResolverT m) where
    liftIO = lift . liftIO

instance Monad m => Alternative (ResolverT m) where
    empty = ResolverT empty
    (ResolverT x) <|> (ResolverT y) = ResolverT $ x <|> y

instance Monad m => MonadPlus (ResolverT m) where
    mzero = empty
    mplus = (<|>)

-- | Retrieves an argument by its name. If the argument with this name couldn't
-- be found, returns 'Null' (i.e. the argument is assumed to
-- be optional then).
argument :: Monad m => Name -> ResolverT m Value
argument argumentName = do
    argumentValue <- ResolverT $ lift $ asks $ lookupArgument . arguments
    pure $ fromMaybe Null argumentValue
  where
    lookupArgument (Arguments argumentMap) =
        HashMap.lookup argumentName argumentMap
