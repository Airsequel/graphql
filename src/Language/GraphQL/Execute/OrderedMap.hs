{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}

-- | This module contains a map data structure, that preserves insertion order.
-- Some definitions conflict with functions from prelude, so this module should
-- probably be imported qualified.
module Language.GraphQL.Execute.OrderedMap
    ( OrderedMap
    , elems
    , empty
    , insert
    , foldlWithKey'
    , keys
    , lookup
    , replace
    , singleton
    , size
    , toList
    , traverseMaybe
    ) where

import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.Key as Key
import qualified Data.Foldable as Foldable
import Data.Maybe
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude hiding (filter, lookup)

-- | This map associates values with the given text keys. Insertion order is
-- preserved. When inserting a value with a key, that is already available in
-- the map, the existing value isn't overridden, but combined with the new value
-- using its 'Semigroup' instance.
--
-- Internally this map uses an array with keys to preserve the order and an
-- unorded map with key-value pairs.
data OrderedMap v = OrderedMap (Vector Text) (KeyMap v)
    deriving (Eq)

instance Functor OrderedMap where
    fmap f (OrderedMap vector keyMap) = OrderedMap vector $ fmap f keyMap

instance Foldable OrderedMap where
    foldr f = foldrWithKey $ const f
    null (OrderedMap vector _) = Vector.null vector

instance Semigroup v => Semigroup (OrderedMap v) where
    (<>) = foldlWithKey'
        $ \accumulator key value -> insert key value accumulator

instance Semigroup v => Monoid (OrderedMap v) where
    mempty = empty

instance Traversable OrderedMap where
    traverse f (OrderedMap vector keyMap) = OrderedMap vector
        <$> traverse f keyMap

instance Show v => Show (OrderedMap v) where
    showsPrec precedence map' = showParen (precedence > 10)
        $ showString "fromList " . shows (toList map')

-- * Construction

-- | Constructs a map with a single element.
singleton :: forall v. Text -> v -> OrderedMap v
singleton key value = OrderedMap (Vector.singleton key)
    $ KeyMap.singleton (Key.fromText key) value

-- | Constructs an empty map.
empty :: forall v. OrderedMap v
empty = OrderedMap mempty mempty

-- * Traversal

-- | Reduces this map by applying a binary operator from right to left to all
-- elements, using the given starting value.
foldrWithKey :: forall v a. (Text -> v -> a -> a) -> a -> OrderedMap v -> a
foldrWithKey f initial (OrderedMap vector keyMap) = foldr go initial vector
  where
    go key = f key (fromJust $ KeyMap.lookup (Key.fromText key) keyMap)

-- | Reduces this map by applying a binary operator from left to right to all
-- elements, using the given starting value.
foldlWithKey' :: forall v a. (a -> Text -> v -> a) -> a -> OrderedMap v -> a
foldlWithKey' f initial (OrderedMap vector keyMap) =
    Vector.foldl' go initial vector
  where
    go accumulator key = f accumulator key
        (fromJust $ KeyMap.lookup (Key.fromText key) keyMap)

-- | Traverse over the elements and collect the 'Just' results.
traverseMaybe
    :: Applicative f
    => forall a
    . (a -> f (Maybe b))
    -> OrderedMap a
    -> f (OrderedMap b)
traverseMaybe f orderedMap = foldlWithKey' filter empty
    <$> traverse f orderedMap
  where
    filter accumulator key (Just value) = replace key value accumulator
    filter accumulator _ Nothing = accumulator

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
insert key value (OrderedMap vector keyMap)
    | Just available <- KeyMap.lookup (Key.fromText key) keyMap = OrderedMap vector
        $ KeyMap.insert (Key.fromText key) (available <> value) keyMap
    | otherwise = OrderedMap (Vector.snoc vector key)
        $ KeyMap.insert (Key.fromText key) value keyMap

-- | Associates the specified value with the specified key in this map. If this
-- map previously contained a mapping for the key, the existing value is
-- replaced by the new one.
replace :: Text -> v -> OrderedMap v -> OrderedMap v
replace key value (OrderedMap vector keyMap)
    | KeyMap.member (Key.fromText key) keyMap = OrderedMap vector
        $ KeyMap.insert (Key.fromText key) value keyMap
    | otherwise = OrderedMap (Vector.snoc vector key)
        $ KeyMap.insert (Key.fromText key) value keyMap

-- | Gives the size of this map, i.e. number of elements in it.
size :: forall v. OrderedMap v -> Int
size (OrderedMap vector _) = Vector.length vector

-- | Looks up a value in this map by key.
lookup :: forall v. Text -> OrderedMap v -> Maybe v
lookup key (OrderedMap _ keyMap) = KeyMap.lookup (Key.fromText key) keyMap
