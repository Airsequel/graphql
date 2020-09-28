{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides the functions to parse and execute @GraphQL@ queries.
module Language.GraphQL
    ( graphql
    , graphqlSubs
    ) where

import Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Language.GraphQL.AST
import Language.GraphQL.Error
import Language.GraphQL.Execute
import qualified Language.GraphQL.Validate as Validate
import Language.GraphQL.Type.Schema (Schema)
import Text.Megaparsec (parse)

-- | If the text parses correctly as a @GraphQL@ query the query is
-- executed using the given 'Schema'.
graphql :: MonadCatch m
    => Schema m -- ^ Resolvers.
    -> Text -- ^ Text representing a @GraphQL@ request document.
    -> m (Either (ResponseEventStream m Aeson.Value) Aeson.Object) -- ^ Response.
graphql schema = graphqlSubs schema mempty mempty

-- | If the text parses correctly as a @GraphQL@ query the substitution is
-- applied to the query and the query is then executed using to the given
-- 'Schema'.
graphqlSubs :: MonadCatch m
    => Schema m -- ^ Resolvers.
    -> Maybe Text -- ^ Operation name.
    -> Aeson.Object -- ^ Variable substitution function.
    -> Text -- ^ Text representing a @GraphQL@ request document.
    -> m (Either (ResponseEventStream m Aeson.Value) Aeson.Object) -- ^ Response.
graphqlSubs schema operationName variableValues document' =
    case parse document "" document' of
        Left errorBundle -> pure . formatResponse <$> parseError errorBundle
        Right parsed ->
            case validate parsed of
                Seq.Empty -> fmap formatResponse
                    <$> execute schema operationName variableValues parsed
                errors -> pure $ pure
                    $ HashMap.singleton "errors"
                    $ Aeson.toJSON
                    $ fromValidationError <$> errors
  where
    validate = Validate.document schema Validate.specifiedRules
    formatResponse (Response data'' Seq.Empty) = HashMap.singleton "data" data''
    formatResponse (Response data'' errors') = HashMap.fromList
        [ ("data", data'')
        , ("errors", Aeson.toJSON $ fromError <$> errors')
        ]
    fromError Error{..} = Aeson.object $ catMaybes
        [ Just ("message", Aeson.toJSON message)
        , toMaybe fromLocation "locations" locations
        , toMaybe fromPath "path" path
        ]
    fromValidationError Validate.Error{..} = Aeson.object
        [ ("message", Aeson.toJSON message)
        , ("locations", Aeson.listValue fromLocation locations)
        ]
    toMaybe _ _ [] = Nothing
    toMaybe f key xs = Just (key, Aeson.listValue f xs)
    fromPath (Segment segment) = Aeson.String segment
    fromPath (Index index) = Aeson.toJSON index
    fromLocation Location{..} = Aeson.object
        [ ("line", Aeson.toJSON line)
        , ("column", Aeson.toJSON column)
        ]
