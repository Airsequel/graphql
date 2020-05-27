{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Error handling.
module Language.GraphQL.Error
    ( parseError
    , CollectErrsT
    , Resolution(..)
    , addErr
    , addErrMsg
    , runCollectErrs
    , singleError
    ) where

import Control.Monad.Trans.State (StateT, modify, runStateT)
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Void (Void)
import Language.GraphQL.AST.Document (Name)
import Language.GraphQL.Type.Schema
import Text.Megaparsec
    ( ParseErrorBundle(..)
    , PosState(..)
    , SourcePos(..)
    , errorOffset
    , parseErrorTextPretty
    , reachOffset
    , unPos
    )

data Resolution m = Resolution
    { errors :: [Aeson.Value]
    , types :: HashMap Name (Type m)
    }

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
type CollectErrsT m = StateT (Resolution m) m

-- | Adds an error to the list of errors.
addErr :: Monad m => Aeson.Value -> CollectErrsT m ()
addErr v = modify appender
  where
    appender resolution@Resolution{..} = resolution{ errors = v : errors }

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

-- | Runs the given query computation, but collects the errors into an error
--   list, which is then sent back with the data.
runCollectErrs :: Monad m
    => HashMap Name (Type m)
    -> CollectErrsT m Aeson.Value
    -> m Aeson.Value
runCollectErrs types' res = do
    (dat, Resolution{..}) <- runStateT res $ Resolution{ errors = [], types = types' }
    if null errors
       then return $ Aeson.object [("data", dat)]
       else return $ Aeson.object
           [ ("data", dat)
           , ("errors", Aeson.toJSON $ reverse errors)
           ]
