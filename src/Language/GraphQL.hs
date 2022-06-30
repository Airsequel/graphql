{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

#ifdef WITH_JSON
-- | This module provides the functions to parse and execute @GraphQL@ queries.
--
-- The content of this module depends on the value of the __json__ flag, which
-- is currently on by default. This behavior will change in the future, the flag
-- will be switched off by default and then removed.
--
-- This documentation is generated with the enabled __json__ flag and functions
-- described here support JSON and are deprecated. JSON instances are provided
-- now by an additional package, __graphql-spice__. To start using the new
-- package create __cabal.project__ in the root directory of your project with
-- the following contents:
--
-- @
-- packages: .
-- constraints: graphql -json
-- @
--
-- Then add __graphql-spice__ as dependency.
--
-- The new version of this module defines only one function, @graphql@, which
-- works with the internal GraphQL value representation used by this lbirary.
-- Refer to @Language.GraphQL.JSON.graphql@ in __graphql-spice__ for the
-- function that accepts and returns JSON.
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
#else
-- | This module provides the functions to parse and execute @GraphQL@ queries.
module Language.GraphQL
    ( graphql
    ) where

import Control.Monad.Catch (MonadCatch)
import Data.HashMap.Strict (HashMap)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.GraphQL.AST as Full
import Language.GraphQL.Error
import Language.GraphQL.Execute
import qualified Language.GraphQL.Validate as Validate
import Language.GraphQL.Type.Schema (Schema)
import Prelude hiding (null)
import Text.Megaparsec (parse)

-- | If the text parses correctly as a @GraphQL@ query the query is
-- executed using the given 'Schema'.
--
-- An operation name can be given if the document contains multiple operations.
graphql :: (MonadCatch m, VariableValue a, Serialize b)
    => Schema m -- ^ Resolvers.
    -> Maybe Text -- ^ Operation name.
    -> HashMap Full.Name a -- ^ Variable substitution function.
    -> Text -- ^ Text representing a @GraphQL@ request document.
    -> m (Either (ResponseEventStream m b) (Response b)) -- ^ Response.
graphql schema operationName variableValues document' =
    case parse Full.document "" document' of
        Left errorBundle -> pure  <$> parseError errorBundle
        Right parsed ->
            case validate parsed of
                Seq.Empty -> execute schema operationName variableValues parsed
                errors -> pure $ pure
                    $ Response null
                    $ fromValidationError <$> errors
  where
    validate = Validate.document schema Validate.specifiedRules
    fromValidationError Validate.Error{..} = Error
        { message = Text.pack message
        , locations = locations
        , path = []
        }
#endif
