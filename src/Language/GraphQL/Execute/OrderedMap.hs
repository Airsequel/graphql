{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
module Language.GraphQL.Execute.OrderedMap
    ( OrderedMap
    , singleton
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data OrderedMap v = OrderedMap (Vector Text) (HashMap Text v)

instance Functor OrderedMap where
    fmap f (OrderedMap vector hashMap) = OrderedMap vector $ fmap f hashMap

instance Foldable OrderedMap where
    foldr f = foldrWithKey $ const f
    null (OrderedMap vector _) = Vector.null vector

instance Show v => Show (OrderedMap v) where
    showsPrec precedence map' = showParen (precedence > 10)
        $ showString "fromList " . shows (toList map')

instance Semigroup (OrderedMap v) where
    (<>) = foldrWithKey go
      where
        go key value accumulator@(OrderedMap vector hashMap)
            | Nothing <- HashMap.lookup key hashMap
                = OrderedMap (Vector.snoc vector key)
                $ HashMap.insert key value hashMap
            | otherwise = accumulator

instance Monoid (OrderedMap v) where
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
    go key accumulator = f key (hashMap HashMap.! key) accumulator

-- * Lists

-- | Converts this map to the list of key-value pairs.
toList :: forall v. OrderedMap v -> [(Text, v)]
toList = foldrWithKey f []
  where
    f key value accumulator = (key, value) : accumulator
