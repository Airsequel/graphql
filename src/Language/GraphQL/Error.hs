{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Error handling.
module Language.GraphQL.Error
    ( parseError
    , CollectErrsT
    , Error(..)
    , Resolution(..)
    , Response(..)
    , addErr
    , addErrMsg
    , runCollectErrs
    , singleError
    ) where

import Control.Monad.Trans.State (StateT, modify, runStateT)
import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Language.GraphQL.AST.Document (Name)
import Language.GraphQL.Execute.Coerce
import Language.GraphQL.Type.Schema
import Prelude hiding (null)
import Text.Megaparsec
    ( ParseErrorBundle(..)
    , PosState(..)
    , SourcePos(..)
    , errorOffset
    , parseErrorTextPretty
    , reachOffset
    , unPos
    )

-- | Executor context.
data Resolution m = Resolution
    { errors :: Seq Error
    , types :: HashMap Name (Type m)
    }

-- | Wraps a parse error into a list of errors.
parseError :: (Applicative f, Serialize a)
    => ParseErrorBundle Text Void
    -> f (Response a)
parseError ParseErrorBundle{..}  =
    pure $ Response null $ fst
        $ foldl go (Seq.empty, bundlePosState) bundleErrors
  where
    errorObject s SourcePos{..} = Error
        (Text.pack $ init $ parseErrorTextPretty s)
        (unPos' sourceLine)
        (unPos' sourceColumn)
    unPos' = fromIntegral . unPos
    go (result, state) x =
        let (_, newState) = reachOffset (errorOffset x) state
            sourcePosition = pstateSourcePos newState
         in (result |> errorObject x sourcePosition, newState)

-- | A wrapper to pass error messages around.
type CollectErrsT m = StateT (Resolution m) m

-- | Adds an error to the list of errors.
addErr :: Monad m => Error -> CollectErrsT m ()
addErr v = modify appender
  where
    appender :: Monad m => Resolution m -> Resolution m
    appender resolution@Resolution{..} = resolution{ errors = errors |> v }

makeErrorMessage :: Text -> Error
makeErrorMessage s = Error s 0 0

-- | Constructs a response object containing only the error with the given
-- message.
singleError :: Serialize a => Text -> Response a
singleError message = Response null $ Seq.singleton $ makeErrorMessage message

-- | Convenience function for just wrapping an error message.
addErrMsg :: Monad m => Text -> CollectErrsT m ()
addErrMsg = addErr . makeErrorMessage

-- | @GraphQL@ error.
data Error = Error
    { message :: Text
    , line :: Word
    , column :: Word
    } deriving (Eq, Show)

-- | The server\'s response describes the result of executing the requested
-- operation if successful, and describes any errors encountered during the
-- request.
data Response a = Response
    { data' :: a
    , errors :: Seq Error
    } deriving (Eq, Show)

-- | Runs the given query computation, but collects the errors into an error
-- list, which is then sent back with the data.
runCollectErrs :: (Monad m, Serialize a)
    => HashMap Name (Type m)
    -> CollectErrsT m a
    -> m (Response a)
runCollectErrs types' res = do
    (dat, Resolution{..}) <- runStateT res
        $ Resolution{ errors = Seq.empty, types = types' }
    pure $ Response dat errors
