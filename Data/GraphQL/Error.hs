{-# LANGUAGE OverloadedStrings #-}
module Data.GraphQL.Error
  ( parseError
  , CollectErrsT
  , addErr
  , addErrMsg
  , runCollectErrs
  , runAppendErrs
  ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text, pack)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State ( StateT
                                 , modify
                                 , runStateT
                                 )

-- | Wraps a parse error into a list of errors.
parseError :: Applicative f => String -> f Aeson.Value
parseError s =
  pure $ Aeson.object [("errors", Aeson.toJSON [makeErrorMsg $ pack s])]

-- | A wrapper to pass error messages around.
type CollectErrsT m = StateT [Aeson.Value] m

-- | Adds an error to the list of errors.
addErr :: Monad m => Aeson.Value -> CollectErrsT m ()
addErr v = modify (v :)

makeErrorMsg :: Text -> Aeson.Value
makeErrorMsg s = Aeson.object [("message", Aeson.toJSON s)]

-- | Convenience function for just wrapping an error message.
addErrMsg :: Monad m => Text -> CollectErrsT m ()
addErrMsg = addErr . makeErrorMsg

-- | Appends the given list of errors to the current list of errors.
appendErrs :: Monad m => [Aeson.Value] -> CollectErrsT m ()
appendErrs errs = modify (errs ++)

-- | Runs the given query computation, but collects the errors into an error
--   list, which is then sent back with the data.
runCollectErrs :: Monad m => CollectErrsT m Aeson.Value -> m Aeson.Value
runCollectErrs res = do
    (dat, errs) <- runStateT res []
    if null errs
       then return $ Aeson.object [("data", dat)]
       else return $ Aeson.object [("data", dat), ("errors", Aeson.toJSON $ reverse errs)]

-- | Runs the given computation, collecting the errors and appending them
--   to the previous list of errors.
runAppendErrs :: Monad m => CollectErrsT m a -> CollectErrsT m a
runAppendErrs f = do
    (v, errs) <- lift $ runStateT f []
    appendErrs errs
    return v
