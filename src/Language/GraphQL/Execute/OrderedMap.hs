{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}

-- | This module contains a map data structure, that preserves insertion order.
module Language.GraphQL.Execute.OrderedMap
    ( OrderedMap
    , elems
    , insert
    , keys
    , lookup
    , singleton
    , size
    ) where

import qualified Data.Foldable as Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude hiding (lookup)

data OrderedMap v = OrderedMap (Vector Text) (HashMap Text v)

instance Functor OrderedMap where
    fmap f (OrderedMap vector hashMap) = OrderedMap vector $ fmap f hashMap

instance Foldable OrderedMap where
    foldr f = foldrWithKey $ const f
    null (OrderedMap vector _) = Vector.null vector

instance Show v => Show (OrderedMap v) where
    showsPrec precedence map' = showParen (precedence > 10)
        $ showString "fromList " . shows (toList map')

instance Semigroup v => Semigroup (OrderedMap v) where
    (<>) = foldrWithKey insert

instance Monoid v => Monoid (OrderedMap v) where
    mempty = OrderedMap mempty mempty

-- * Construction

-- | Constructs a map with a single element.
singleton :: forall v. Text -> v -> OrderedMap v
singleton key value = OrderedMap (Vector.singleton key)
    $ HashMap.singleton key value

-- * Folds

-- | Reduces this map by applying a binary operator to all elements, using the
-- given starting value.
foldrWithKey :: forall v a. (Text -> v -> a -> a) -> a -> OrderedMap v -> a
foldrWithKey f initial (OrderedMap vector hashMap) = foldr go initial vector
  where
    go key = f key (hashMap HashMap.! key)

-- * Lists

-- | Converts this map to the list of key-value pairs.
toList :: forall v. OrderedMap v -> [(Text, v)]
toList = foldrWithKey ((.) (:) . (,)) []

-- | Returns a list with all keys in this map.
keys :: forall v. OrderedMap v -> [Text]
keys (OrderedMap vector _) = Foldable.toList vector

-- | Returns a list with all elements in this map.
elems :: forall v. OrderedMap v -> [v]
elems = fmap snd . toList

-- * Basic interface

-- | Associates the specified value with the specified key in this map. If this
-- map previously contained a mapping for the key, the existing and new values
-- are combined.
insert :: Semigroup v => Text -> v -> OrderedMap v -> OrderedMap v
insert key value (OrderedMap vector hashMap)
    | Just available <- HashMap.lookup key hashMap = OrderedMap vector
        $ HashMap.insert key (available <> value) hashMap
    | otherwise = OrderedMap (Vector.snoc vector key)
        $ HashMap.insert key value hashMap

-- | Gives the size of this map, i.e. number of elements in it.
size :: forall v. OrderedMap v -> Int
size (OrderedMap vector _) = Vector.length vector

-- | Looks up a value in this map by key.
lookup :: forall v. Text -> OrderedMap v -> Maybe v
lookup key (OrderedMap _ hashMap) = HashMap.lookup key hashMap
