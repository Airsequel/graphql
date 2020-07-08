{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides the functions to parse and execute @GraphQL@ queries.
module Language.GraphQL
    ( graphql
    , graphqlSubs
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Language.GraphQL.AST
import Language.GraphQL.Error
import Language.GraphQL.Execute
import Language.GraphQL.Type.Schema
import Text.Megaparsec (parse)

-- | If the text parses correctly as a @GraphQL@ query the query is
-- executed using the given 'Schema'.
graphql :: Monad m
    => Schema m -- ^ Resolvers.
    -> Text -- ^ Text representing a @GraphQL@ request document.
    -> m Aeson.Value -- ^ Response.
graphql schema = graphqlSubs schema mempty mempty

-- | If the text parses correctly as a @GraphQL@ query the substitution is
-- applied to the query and the query is then executed using to the given
-- 'Schema'.
graphqlSubs :: Monad m
    => Schema m -- ^ Resolvers.
    -> Maybe Text -- ^ Operation name.
    -> Aeson.Object -- ^ Variable substitution function.
    -> Text -- ^ Text representing a @GraphQL@ request document.
    -> m Aeson.Value -- ^ Response.
graphqlSubs schema operationName variableValues document' =
    either parseError executeRequest parsed >>= formatResponse
  where
    parsed = parse document "" document'
    formatResponse (Response data'' Seq.Empty) =
        pure $ Aeson.object [("data", data'')]
    formatResponse (Response data'' errors') = pure $ Aeson.object
        [ ("data", data'')
        , ("errors", Aeson.toJSON $ fromError <$> errors')
        ]
    fromError Error{ locations = [], ..} =
        Aeson.object [("message", Aeson.toJSON message)]
    fromError Error{..} = Aeson.object
        [ ("message", Aeson.toJSON message)
        , ("locations", Aeson.listValue fromLocation locations)
        ]
    fromLocation Location{..} = Aeson.object
        [ ("line", Aeson.toJSON line)
        , ("column", Aeson.toJSON column)
        ]
    executeRequest = execute schema operationName variableValues
