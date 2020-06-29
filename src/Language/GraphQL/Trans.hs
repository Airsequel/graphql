-- | Monad transformer stack used by the @GraphQL@ resolvers.
module Language.GraphQL.Trans
    ( argument
    , ResolverT(..)
    , Context(..)
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, asks)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.GraphQL.AST (Name)
import Language.GraphQL.AST.Core
import Language.GraphQL.Type.Definition
import Prelude hiding (lookup)

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
--   be found, returns 'Null' (i.e. the argument is assumed to
--   be optional then).
argument :: Monad m => Name -> ResolverT m Value
argument argumentName = do
    argumentValue <- ResolverT $ lift $ asks $ lookup . arguments
    pure $ fromMaybe Null argumentValue
  where
    lookup (Arguments argumentMap) = HashMap.lookup argumentName argumentMap
