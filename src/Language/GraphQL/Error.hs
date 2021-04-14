{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

-- | Error handling.
module Language.GraphQL.Error
    ( CollectErrsT
    , Error(..)
    , Path(..)
    , Resolution(..)
    , ResolverException(..)
    , Response(..)
    , ResponseEventStream
    , addErr
    , addErrMsg
    , parseError
    , runCollectErrs
    , singleError
    ) where

import Conduit
import Control.Exception (Exception(..))
import Control.Monad.Trans.State (StateT, modify, runStateT)
import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST (Location(..), Name)
import Language.GraphQL.Execute.Coerce
import qualified Language.GraphQL.Type.Schema as Schema
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
    , types :: HashMap Name (Schema.Type m)
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
        { message = Text.pack $ init $ parseErrorTextPretty s
        , locations = [Location (unPos' sourceLine) (unPos' sourceColumn)]
        , path = []
        }
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
makeErrorMessage s = Error s [] []

-- | Constructs a response object containing only the error with the given
-- message.
singleError :: Serialize a => Text -> Response a
singleError message = Response null $ Seq.singleton $ makeErrorMessage message

-- | Convenience function for just wrapping an error message.
addErrMsg :: (Monad m, Serialize a) => Text -> CollectErrsT m a
addErrMsg errorMessage = (addErr . makeErrorMessage) errorMessage >> pure null

-- | If an error can be associated to a particular field in the GraphQL result,
-- it must contain an entry with the key path that details the path of the
-- response field which experienced the error. This allows clients to identify
-- whether a null result is intentional or caused by a runtime error.
data Path
    = Segment Text -- ^ Field name.
    | Index Int -- ^ List index if a field returned a list.
    deriving (Eq, Show)

-- | @GraphQL@ error.
data Error = Error
    { message :: Text
    , locations :: [Location]
    , path :: [Path]
    } deriving (Eq, Show)

-- | The server\'s response describes the result of executing the requested
-- operation if successful, and describes any errors encountered during the
-- request.
data Response a = Response
    { data' :: a
    , errors :: Seq Error
    } deriving (Eq, Show)

-- | Each event in the underlying Source Stream triggers execution of the
-- subscription selection set. The results of the execution generate a Response
-- Stream.
type ResponseEventStream m a = ConduitT () (Response a) m ()

-- | Only exceptions that inherit from 'ResolverException' a cought by the
-- executor.
data ResolverException = forall e. Exception e => ResolverException e

instance Show ResolverException where
    show (ResolverException e) = show e

instance Exception ResolverException

-- | Runs the given query computation, but collects the errors into an error
-- list, which is then sent back with the data.
runCollectErrs :: (Monad m, Serialize a)
    => HashMap Name (Schema.Type m)
    -> CollectErrsT m a
    -> m (Response a)
runCollectErrs types' res = do
    (dat, Resolution{..}) <- runStateT res
        $ Resolution{ errors = Seq.empty, types = types' }
    pure $ Response dat errors
