{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.GraphQL.Execute.Internal
    ( addError
    , singleError
    ) where

import Control.Monad.Trans.State (modify)
import Control.Monad.Catch (MonadCatch)
import Data.Sequence ((|>))
import Data.Text (Text)
import Language.GraphQL.Execute.Coerce
import Language.GraphQL.Error
    ( CollectErrsT
    , Error(..)
    , Resolution(..)
    , Response(..)
    )
import Prelude hiding (null)

addError :: MonadCatch m => forall a. a -> Error -> CollectErrsT m a
addError returnValue error' = modify appender >> pure returnValue
  where
   appender :: Resolution m -> Resolution m
   appender resolution@Resolution{ errors } = resolution
       { errors = errors |> error'
       }

singleError :: Serialize b => forall a. Text -> Either a (Response b)
singleError message = Right $ Response null $ pure $ Error message [] []
