{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}

-- | GraphQL validator.
module Language.GraphQL.Validate
    ( Error(..)
    , Path(..)
    , document
    , module Language.GraphQL.Validate.Rules
    ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Reader (Reader, asks, mapReaderT, runReader)
import Data.Foldable (foldrM)
import Data.Sequence (Seq(..), (><), (|>))
import qualified Data.Sequence as Seq
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Internal
import Language.GraphQL.Type.Schema (Schema(..))
import Language.GraphQL.Validate.Rules
import Language.GraphQL.Validate.Validation

type ValidateT m = Reader (Validation m) (Seq Error)

-- | Validates a document and returns a list of found errors. If the returned
-- list is empty, the document is valid.
document :: forall m. Schema m -> [Rule m] -> Document -> Seq Error
document schema' rules' document' =
    runReader (foldrM go Seq.empty document') context
  where
    context = Validation
        { ast = document'
        , schema = schema'
        , types = collectReferencedTypes schema'
        , rules = rules'
        }
    go definition' accumulator = (accumulator ><) <$> definition definition'

definition :: forall m. Definition -> ValidateT m
definition = \case
    definition'@(ExecutableDefinition executableDefinition') -> do
        applied <- applyRules definition'
        children <- executableDefinition executableDefinition'
        pure $ children >< applied
    definition' -> applyRules definition'
  where
    applyRules definition' =
        asks rules >>= foldM (ruleFilter definition') Seq.empty
    ruleFilter definition' accumulator (DefinitionRule rule) =
        mapReaderT (runRule accumulator) $ rule definition'
    ruleFilter _ accumulator _ = pure accumulator

runRule :: Applicative f => Seq Error -> Maybe Error -> f (Seq Error)
runRule accumulator (Just error') = pure $ accumulator |> error'
runRule accumulator Nothing = pure accumulator

executableDefinition :: forall m. ExecutableDefinition -> ValidateT m
executableDefinition (DefinitionOperation definition') =
    operationDefinition definition'
executableDefinition (DefinitionFragment definition') =
    fragmentDefinition definition'

operationDefinition :: forall m. OperationDefinition -> ValidateT m
operationDefinition operation =
    asks rules >>= foldM (ruleFilter operation) Seq.empty
  where
    ruleFilter definition' accumulator (OperationDefinitionRule rule) =
        mapReaderT (runRule accumulator) $ rule definition'
    ruleFilter _ accumulator _ = pure accumulator

fragmentDefinition :: forall m. FragmentDefinition -> ValidateT m
fragmentDefinition _fragment = pure Seq.empty
