-- | Monad transformer stack used by the @GraphQL@ resolvers.
module Language.GraphQL.Trans
    ( ActionT(..)
    , Context(..)
    , argument
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
import Language.GraphQL.AST.Core
import qualified Language.GraphQL.Type.In as In
import Prelude hiding (lookup)

-- | Resolution context holds resolver arguments.
data Context = Context
    { arguments :: Arguments
    , info :: Field
    }

-- | Monad transformer stack used by the resolvers to provide error handling
--   and resolution context (resolver arguments).
newtype ActionT m a = ActionT
    { runActionT :: ExceptT Text (ReaderT Context m) a
    }

instance Functor m => Functor (ActionT m) where
    fmap f = ActionT . fmap f . runActionT

instance Monad m => Applicative (ActionT m) where
    pure = ActionT . pure
    (ActionT f) <*> (ActionT x) = ActionT $ f <*> x

instance Monad m => Monad (ActionT m) where
    return = pure
    (ActionT action) >>= f = ActionT $ action >>= runActionT . f

instance MonadTrans ActionT where
    lift = ActionT . lift . lift

instance MonadIO m => MonadIO (ActionT m) where
    liftIO = lift . liftIO

instance Monad m => Alternative (ActionT m) where
    empty = ActionT empty
    (ActionT x) <|> (ActionT y) = ActionT $ x <|> y

instance Monad m => MonadPlus (ActionT m) where
    mzero = empty
    mplus = (<|>)

-- | Retrieves an argument by its name. If the argument with this name couldn't
--   be found, returns 'In.Null' (i.e. the argument is assumed to
--   be optional then).
argument :: Monad m => Name -> ActionT m In.Value
argument argumentName = do
    argumentValue <- ActionT $ lift $ asks $ lookup . arguments
    pure $ fromMaybe In.Null argumentValue
  where
    lookup (Arguments argumentMap) = HashMap.lookup argumentName argumentMap
