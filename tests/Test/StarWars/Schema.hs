{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars.Schema
    ( character
    , droid
    , hero
    , human
    , schema
    ) where

import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import Language.GraphQL.Schema ( Schema
                               , Resolver
                               , Argument(..)
                               , Value(..)
                               )
import qualified Language.GraphQL.Schema as Schema
import Language.GraphQL.Trans
import Language.GraphQL.Type
import Test.StarWars.Data

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: MonadIO m => Schema m
schema = hero :| [human, droid]

hero :: MonadIO m => Resolver m
hero = Schema.objectA "hero" $ \case
  [] -> character artoo
  [Argument "episode" (ValueEnum "NEWHOPE")] -> character $ getHero 4
  [Argument "episode" (ValueEnum "EMPIRE" )] -> character $ getHero 5
  [Argument "episode" (ValueEnum "JEDI"   )] -> character $ getHero 6
  _ -> ActionT $ throwE "Invalid arguments."

human :: MonadIO m => Resolver m
human = Schema.wrappedObjectA "human" $ \case
  [Argument "id" (ValueString i)] -> do
      humanCharacter <- lift $ return $ getHuman i >>= Just
      case humanCharacter of
        Nothing -> return Null
        Just e -> Named <$> character e
  _ -> ActionT $ throwE "Invalid arguments."

droid :: MonadIO m => Resolver m
droid = Schema.objectA "droid" $ \case
   [Argument "id" (ValueString i)] -> character =<< liftIO (getDroid i)
   _ -> ActionT $ throwE "Invalid arguments."

character :: MonadIO m => Character -> ActionT m [Resolver m]
character char = return
    [ Schema.scalar "id" $ return $ id_ char
    , Schema.scalar "name" $ return $ name char
    , Schema.wrappedObject "friends"
        $ traverse character $ List $ Named <$> getFriends char
    , Schema.enum "appearsIn" $ return $ foldMap getEpisode $ appearsIn char
    , Schema.scalar "secretBackstory" $ secretBackstory char
    , Schema.scalar "homePlanet" $ return $ either mempty homePlanet char
    , Schema.scalar "__typename" $ return $ typeName char
    ]
