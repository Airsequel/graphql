-- | This module provides the functions to parse and execute @GraphQL@ queries.
module Language.GraphQL
    ( graphql
    , graphqlSubs
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Language.GraphQL.Error
import Language.GraphQL.Execute
import Language.GraphQL.AST.Parser
import qualified Language.GraphQL.Schema as Schema
import Text.Megaparsec (parse)

-- | If the text parses correctly as a @GraphQL@ query the query is
-- executed using the given 'Schema.Resolver's.
graphql :: MonadIO m
    => NonEmpty (Schema.Resolver m) -- ^ Resolvers.
    -> T.Text -- ^ Text representing a @GraphQL@ request document.
    -> m Aeson.Value -- ^ Response.
graphql = flip graphqlSubs $ const Nothing

-- | If the text parses correctly as a @GraphQL@ query the substitution is
-- applied to the query and the query is then executed using to the given
-- 'Schema.Resolver's.
graphqlSubs :: MonadIO m
    => NonEmpty (Schema.Resolver m) -- ^ Resolvers.
    -> Schema.Subs -- ^ Variable substitution function.
    -> T.Text -- ^ Text representing a @GraphQL@ request document.
    -> m Aeson.Value -- ^ Response.
graphqlSubs schema f
    = either parseError (execute schema f)
    . parse document ""
