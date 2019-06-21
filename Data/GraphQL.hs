-- | This module provides the functions to parse and execute @GraphQL@ queries.
module Data.GraphQL where

import Control.Applicative (Alternative)

import qualified Data.Text as T

import qualified Data.Aeson as Aeson
import Text.Megaparsec ( errorBundlePretty
                       , parse
                       )

import Data.GraphQL.Execute
import Data.GraphQL.Parser
import Data.GraphQL.Schema

import Data.GraphQL.Error

-- | Takes a 'Schema' and text representing a @GraphQL@ request document.
--   If the text parses correctly as a @GraphQL@ query the query is
--   executed according to the given 'Schema'.
--
--   Returns the response as an @Aeson.@'Aeson.Value'.
graphql :: (Alternative m, Monad m) => Schema m -> T.Text -> m Aeson.Value
graphql = flip graphqlSubs $ const Nothing

-- | Takes a 'Schema', a variable substitution function and text
--   representing a @GraphQL@ request document.  If the text parses
--   correctly as a @GraphQL@ query the substitution is applied to the
--   query and the query is then executed according to the given 'Schema'.
--
--   Returns the response as an @Aeson.@'Aeson.Value'.
graphqlSubs :: (Alternative m, Monad m) => Schema m -> Subs -> T.Text -> m Aeson.Value
graphqlSubs schema f =
    either (parseError . errorBundlePretty) (execute schema f)
    . parse document ""
