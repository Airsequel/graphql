{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.Execute.Subscribe
    ( subscribe
    ) where

import Conduit
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Language.GraphQL.AST (Name)
import Language.GraphQL.Execute.Coerce
import Language.GraphQL.Execute.Execution
import qualified Language.GraphQL.Execute.Transform as Transform
import Language.GraphQL.Error
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema

-- This is actually executeMutation, but we don't distinguish between queries
-- and mutations yet.
subscribe :: (Monad m, Serialize a)
    => HashMap Name (Type m)
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> m (Either Text (ResponseEventStream m a))
subscribe types' objectType fields = do
    sourceStream <- createSourceEventStream types' objectType fields
    traverse (mapSourceToResponseEvent types' objectType fields) sourceStream

mapSourceToResponseEvent :: (Monad m, Serialize a)
    => HashMap Name (Type m)
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> Out.SourceEventStream m
    -> m (ResponseEventStream m a)
mapSourceToResponseEvent types' subscriptionType fields sourceStream = pure
    $ sourceStream
    .| mapMC (executeSubscriptionEvent types' subscriptionType fields)

createSourceEventStream :: Monad m
    => HashMap Name (Type m)
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> m (Either Text (Out.SourceEventStream m))
createSourceEventStream _types subscriptionType@(Out.ObjectType _ _ _ fieldTypes) fields
    | [fieldGroup] <- Map.elems groupedFieldSet
    , Transform.Field _ fieldName arguments' _ <- NonEmpty.head fieldGroup
    , resolverT <- fieldTypes HashMap.! fieldName
    , Out.EventStreamResolver fieldDefinition _ resolver <- resolverT
    , Out.Field _ _fieldType argumentDefinitions <- fieldDefinition =
        case coerceArgumentValues argumentDefinitions arguments' of
            Nothing -> pure $ Left "Argument coercion failed."
            Just argumentValues ->
                resolveFieldEventStream Type.Null argumentValues resolver
    | otherwise = pure $ Left "Subscription contains more than one field."
  where
    groupedFieldSet = collectFields subscriptionType fields

resolveFieldEventStream :: Monad m
    => Type.Value
    -> Type.Subs
    -> ExceptT Text (ReaderT Out.Context m) (Out.SourceEventStream m)
    -> m (Either Text (Out.SourceEventStream m))
resolveFieldEventStream result args resolver =
    flip runReaderT context $ runExceptT resolver
  where
    context = Type.Context
        { Type.arguments = Type.Arguments args
        , Type.values = result
        }

-- This is actually executeMutation, but we don't distinguish between queries
-- and mutations yet.
executeSubscriptionEvent :: (Monad m, Serialize a)
    => HashMap Name (Type m)
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> Definition.Value
    -> m (Response a)
executeSubscriptionEvent types' objectType fields initialValue =
    runCollectErrs types' $ executeSelectionSet initialValue objectType fields
