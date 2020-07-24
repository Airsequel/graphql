{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

-- | This module contains default rules defined in the GraphQL specification.
module Language.GraphQL.Validate.Rules
    ( Rule(..)
    , executableDefinitionsRule
    , specifiedRules
    ) where

import Language.GraphQL.AST.Document

-- | 'Rule' assigns a function to each AST node that can be validated. If the
-- validation fails, the function should return an error message, or 'Nothing'
-- otherwise.
newtype Rule
    = DefinitionRule (Definition -> Maybe String)

-- | Default reules given in the specification.
specifiedRules :: [Rule]
specifiedRules =
    [ executableDefinitionsRule
    ]

-- | Definition must be OperationDefinition or FragmentDefinition.
executableDefinitionsRule :: Rule
executableDefinitionsRule = DefinitionRule go
  where
    go (ExecutableDefinition _definition _) = Nothing
    go _ = Just "Definition must be OperationDefinition or FragmentDefinition."
