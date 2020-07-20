{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

module Language.GraphQL.Validate.Rules
    ( Rule(..)
    , executableDefinitionsRule
    , specifiedRules
    ) where

import Language.GraphQL.AST.Document

newtype Rule
    = DefinitionRule (Definition -> Maybe String)

specifiedRules :: [Rule]
specifiedRules =
    [ executableDefinitionsRule
    ]

executableDefinitionsRule :: Rule
executableDefinitionsRule = DefinitionRule go
  where
    go (ExecutableDefinition _definition _) = Nothing
    go _ = Just "Definition must be OperationDefinition or FragmentDefinition."
