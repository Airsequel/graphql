-- | This module provides the functions to parse and execute @GraphQL@ queries.
module Language.GraphQL where

import Control.Monad (MonadPlus)

import qualified Data.Text as T

import qualified Data.Aeson as Aeson
import Text.Megaparsec ( errorBundlePretty
                       , parse
                       )

import Language.GraphQL.Execute
import Language.GraphQL.Parser
import Language.GraphQL.Schema

import Language.GraphQL.Error

-- | Takes a 'Schema' and text representing a @GraphQL@ request document.
--   If the text parses correctly as a @GraphQL@ query the query is
--   executed according to the given 'Schema'.
--
--   Returns the response as an @Aeson.@'Aeson.Value'.
graphql :: MonadPlus m => Schema m -> T.Text -> m Aeson.Value
graphql = flip graphqlSubs $ const Nothing

-- | Takes a 'Schema', a variable substitution function and text
--   representing a @GraphQL@ request document.  If the text parses
--   correctly as a @GraphQL@ query the substitution is applied to the
--   query and the query is then executed according to the given 'Schema'.
--
--   Returns the response as an @Aeson.@'Aeson.Value'.
graphqlSubs :: MonadPlus m => Schema m -> Subs -> T.Text -> m Aeson.Value
graphqlSubs schema f =
    either (parseError . errorBundlePretty) (execute schema f)
    . parse document ""
