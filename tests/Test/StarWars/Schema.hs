{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars.Schema where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus)
import Data.List.NonEmpty (NonEmpty((:|)))

import Data.GraphQL.Schema ( Schema
                           , Resolver
                           , Argument(..)
                           , Value(..)
                           )
import qualified Data.GraphQL.Schema as Schema

import Test.StarWars.Data

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: MonadPlus m => Schema m
schema = hero :| [human, droid]

hero :: MonadPlus m => Resolver m
hero = Schema.objectA "hero" $ \case
  [] -> character artoo
  [Argument "episode" (ValueInt n)]   -> character . getHero $ fromIntegral n
  [Argument "episode" (ValueEnum "NEWHOPE")] -> character $ getHero 4
  [Argument "episode" (ValueEnum "EMPIRE" )] -> character $ getHero 5
  [Argument "episode" (ValueEnum "JEDI"   )] -> character $ getHero 6
  _ -> empty

human :: MonadPlus m => Resolver m
human = Schema.objectA "human" $ \case
  [Argument "id" (ValueString i)] -> character =<< getHuman i
  _ -> empty

droid :: MonadPlus m => Resolver m
droid = Schema.objectA "droid" $ \case
   [Argument "id" (ValueString i)] -> character =<< getDroid i
   _ -> empty

character :: MonadPlus m => Character -> [Resolver m]
character char =
  [ Schema.scalar "id"              $ id_ char
  , Schema.scalar "name"            $ name char
  , Schema.array  "friends"         $ character <$> getFriends char
  , Schema.enum   "appearsIn"       . traverse getEpisode $ appearsIn char
  , Schema.scalar "secretBackstory" $ secretBackstory char
  , Schema.scalar "homePlanet"      $ either mempty homePlanet char
  , Schema.scalar "__typename"      $ typeName char
  ]
