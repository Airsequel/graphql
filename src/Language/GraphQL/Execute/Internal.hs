{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.GraphQL.Execute.Internal
    ( addError
    ) where

import Control.Monad.Trans.State (modify)
import Control.Monad.Catch (MonadCatch)
import Data.Sequence ((|>))
import Language.GraphQL.Error
import Language.GraphQL.Execute.Coerce
import Prelude hiding (null)

addError :: (Serialize a, MonadCatch m) => Error -> CollectErrsT m a
addError error' = modify appender >> pure null
  where
   appender :: Resolution m -> Resolution m
   appender resolution@Resolution{ errors } = resolution
       { errors = errors |> error'
       }
