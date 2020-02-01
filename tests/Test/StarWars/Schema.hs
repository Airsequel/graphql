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
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes)
import qualified Language.GraphQL.Schema as Schema
import Language.GraphQL.Trans
import qualified Language.GraphQL.Type as Type
import Test.StarWars.Data

-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: NonEmpty (Schema.Resolver Identity)
schema = hero :| [human, droid]

hero :: Schema.Resolver Identity
hero = Schema.object "hero" $ do
  episode <- argument "episode"
  character $ case episode of
      Schema.Enum "NEWHOPE" -> getHero 4
      Schema.Enum "EMPIRE" -> getHero 5
      Schema.Enum "JEDI" -> getHero 6
      _ -> artoo

human :: Schema.Resolver Identity
human = Schema.wrappedObject "human" $ do
    id' <- argument "id"
    case id' of
        Schema.String i -> do
            humanCharacter <- lift $ return $ getHuman i >>= Just
            case humanCharacter of
                Nothing -> return Type.Null
                Just e -> Type.Named <$> character e
        _ -> ActionT $ throwE "Invalid arguments."

droid :: Schema.Resolver Identity
droid = Schema.object "droid" $ do
    id' <- argument "id"
    case id' of
        Schema.String i -> character =<< getDroid i
        _ -> ActionT $ throwE "Invalid arguments."

character :: Character -> ActionT Identity [Schema.Resolver Identity]
character char = return
    [ Schema.scalar "id" $ return $ id_ char
    , Schema.scalar "name" $ return $ name char
    , Schema.wrappedObject "friends"
        $ traverse character $ Type.List $ Type.Named <$> getFriends char
    , Schema.wrappedScalar "appearsIn" $ return . Type.List
        $ catMaybes (getEpisode <$> appearsIn char)
    , Schema.scalar "secretBackstory" $ secretBackstory char
    , Schema.scalar "homePlanet" $ return $ either mempty homePlanet char
    , Schema.scalar "__typename" $ return $ typeName char
    ]
