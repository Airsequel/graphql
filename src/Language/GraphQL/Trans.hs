-- | Monad transformer stack used by the @GraphQL@ resolvers.
module Language.GraphQL.Trans
    ( ActionT(..)
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)

-- | Monad transformer stack used by the resolvers to provide error handling.
newtype ActionT m a = ActionT { runActionT :: ExceptT Text m a }

instance Functor m => Functor (ActionT m) where
    fmap f = ActionT . fmap f . runActionT

instance Monad m => Applicative (ActionT m) where
    pure = ActionT . pure
    (ActionT f) <*> (ActionT x) = ActionT $ f <*> x

instance Monad m => Monad (ActionT m) where
    return = pure
    (ActionT action) >>= f = ActionT $ action >>= runActionT . f

instance MonadTrans ActionT where
    lift = ActionT . lift

instance MonadIO m => MonadIO (ActionT m) where
    liftIO = lift . liftIO

instance Monad m => Alternative (ActionT m) where
    empty = ActionT empty
    (ActionT x) <|> (ActionT y) = ActionT $ x <|> y

instance Monad m => MonadPlus (ActionT m) where
    mzero = empty
    mplus = (<|>)
