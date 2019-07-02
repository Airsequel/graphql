{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars.Data where

import Data.Monoid (mempty)
import Control.Applicative (liftA2)
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Except (throwE)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Language.GraphQL.Trans

-- * Data
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsData.js

-- ** Characters

type ID = Text

data CharCommon = CharCommon
  { _id_        :: ID
  , _name       :: Text
  , _friends    :: [ID]
  , _appearsIn  :: [Int]
  } deriving (Show)


data Human = Human
  { _humanChar  :: CharCommon
  , homePlanet :: Text
  }

data Droid = Droid
  { _droidChar       :: CharCommon
  , primaryFunction :: Text
  }

type Character = Either Droid Human

id_ :: Character -> ID
id_ (Left  x) = _id_ . _droidChar $ x
id_ (Right x) = _id_ . _humanChar $ x

name :: Character -> Text
name (Left  x) = _name . _droidChar $ x
name (Right x) = _name . _humanChar $ x

friends :: Character -> [ID]
friends (Left  x) = _friends . _droidChar $ x
friends (Right x) = _friends . _humanChar $ x

appearsIn :: Character -> [Int]
appearsIn (Left  x) = _appearsIn . _droidChar $ x
appearsIn (Right x) = _appearsIn . _humanChar $ x

secretBackstory :: MonadPlus m => Character -> ActionT m Text
secretBackstory = const $ ActionT $ throwE "secretBackstory is secret."

typeName :: Character -> Text
typeName = either (const "Droid") (const "Human")

luke :: Character
luke = Right luke'

luke' :: Human
luke' = Human
  { _humanChar = CharCommon
      { _id_        = "1000"
      , _name       = "Luke Skywalker"
      , _friends    = ["1002","1003","2000","2001"]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = "Tatooine"
  }

vader :: Human
vader = Human
  { _humanChar = CharCommon
      { _id_        = "1001"
      , _name       = "Darth Vader"
      , _friends    = ["1004"]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = "Tatooine"
  }

han :: Human
han = Human
  { _humanChar = CharCommon
      { _id_        = "1002"
      , _name       = "Han Solo"
      , _friends    = ["1000","1003","2001" ]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = mempty
  }

leia :: Human
leia = Human
  { _humanChar = CharCommon
      { _id_        = "1003"
      , _name       = "Leia Organa"
      , _friends    = ["1000","1002","2000","2001"]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = "Alderaan"
  }

tarkin :: Human
tarkin = Human
  { _humanChar = CharCommon
    { _id_        = "1004"
    , _name       = "Wilhuff Tarkin"
    , _friends    = ["1001"]
    , _appearsIn  = [4]
    }
  , homePlanet = mempty
  }

threepio :: Droid
threepio = Droid
  { _droidChar = CharCommon
      { _id_ = "2000"
      , _name = "C-3PO"
      , _friends = ["1000","1002","1003","2001" ]
      , _appearsIn = [ 4, 5, 6 ]
      }
  , primaryFunction = "Protocol"
  }

artoo :: Character
artoo = Left artoo'

artoo' :: Droid
artoo' = Droid
  { _droidChar = CharCommon
      { _id_        = "2001"
      , _name       = "R2-D2"
      , _friends    = ["1000","1002","1003"]
      , _appearsIn  = [4,5,6]
      }
  , primaryFunction = "Astrometch"
  }

-- ** Helper functions

getHero :: Int -> Character
getHero 5 = luke
getHero _ = artoo

getHeroIO :: Int -> IO Character
getHeroIO = pure . getHero

getHuman :: MonadPlus m => ID -> m Character
getHuman = fmap Right . getHuman'

getHuman' :: MonadPlus m => ID -> m Human
getHuman' "1000" = pure luke'
getHuman' "1001" = pure vader
getHuman' "1002" = pure han
getHuman' "1003" = pure leia
getHuman' "1004" = pure tarkin
getHuman' _      = mzero

getDroid :: MonadPlus m => ID -> m Character
getDroid = fmap Left . getDroid'

getDroid' :: MonadPlus m => ID -> m Droid
getDroid' "2000" = pure threepio
getDroid' "2001" = pure artoo'
getDroid' _      = mzero

getFriends :: Character -> [Character]
getFriends char = catMaybes $ liftA2 mplus getDroid getHuman <$> friends char

getEpisode :: MonadPlus m => Int -> m Text
getEpisode 4 = pure "NEWHOPE"
getEpisode 5 = pure "EMPIRE"
getEpisode 6 = pure "JEDI"
getEpisode _ = mzero
