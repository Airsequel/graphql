{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Error handling.
module Language.GraphQL.Error
    ( parseError
    , CollectErrsT
    , addErr
    , addErrMsg
    , runCollectErrs
    , runAppendErrs
    , singleError
    ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Void (Void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State ( StateT
                                 , modify
                                 , runStateT
                                 )
import Text.Megaparsec
    ( ParseErrorBundle(..)
    , PosState(..)
    , SourcePos(..)
    , errorOffset
    , parseErrorTextPretty
    , reachOffset
    , unPos
    )

-- | Wraps a parse error into a list of errors.
parseError :: Applicative f => ParseErrorBundle Text Void -> f Aeson.Value
parseError ParseErrorBundle{..}  =
    pure $ Aeson.object [("errors", Aeson.toJSON $ fst $ foldl go ([], bundlePosState) bundleErrors)]
  where
    errorObject s SourcePos{..} = Aeson.object
        [ ("message", Aeson.toJSON $ init $ parseErrorTextPretty s)
        , ("line", Aeson.toJSON $ unPos sourceLine)
        , ("column", Aeson.toJSON $ unPos sourceColumn)
        ]
    go (result, state) x =
        let (_, newState) = reachOffset (errorOffset x) state
            sourcePosition = pstateSourcePos newState
         in (errorObject x sourcePosition : result, newState)

-- | A wrapper to pass error messages around.
type CollectErrsT m = StateT [Aeson.Value] m

-- | Adds an error to the list of errors.
addErr :: Monad m => Aeson.Value -> CollectErrsT m ()
addErr v = modify (v :)

makeErrorMessage :: Text -> Aeson.Value
makeErrorMessage s = Aeson.object [("message", Aeson.toJSON s)]

-- | Constructs a response object containing only the error with the given
--   message.
singleError :: Text -> Aeson.Value
singleError message = Aeson.object
    [ ("errors", Aeson.toJSON [makeErrorMessage message])
    ]

-- | Convenience function for just wrapping an error message.
addErrMsg :: Monad m => Text -> CollectErrsT m ()
addErrMsg = addErr . makeErrorMessage

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
