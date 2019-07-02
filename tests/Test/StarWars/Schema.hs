{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars.Schema where

import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class (lift)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.GraphQL.Schema ( Schema
                           , Resolver
                           , Argument(..)
                           , Value(..)
                           )
import qualified Data.GraphQL.Schema as Schema
import Language.GraphQL.Trans
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
  _ -> ActionT $ throwE "Invalid arguments."

human :: MonadPlus m => Resolver m
human = Schema.objectA "human" $ \case
  [Argument "id" (ValueString i)] -> character =<< lift (getHuman i)
  _ -> ActionT $ throwE "Invalid arguments."

droid :: MonadPlus m => Resolver m
droid = Schema.objectA "droid" $ \case
   [Argument "id" (ValueString i)] -> character =<< lift (getDroid i)
   _ -> ActionT $ throwE "Invalid arguments."

character :: MonadPlus m => Character -> ActionT m [Resolver m]
character char = return
    [ Schema.scalar "id"              $ return $ id_ char
    , Schema.scalar "name"            $ return $ name char
    , Schema.array  "friends"         $ traverse character $ getFriends char
    , Schema.enum   "appearsIn"       $ return $ foldMap getEpisode $ appearsIn char
    , Schema.scalar "secretBackstory" $ secretBackstory char
    , Schema.scalar "homePlanet"      $ return $ either mempty homePlanet char
    , Schema.scalar "__typename"      $ return $ typeName char
    ]
