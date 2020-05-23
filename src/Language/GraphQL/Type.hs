-- | Definitions for @GraphQL@ input types.
module Language.GraphQL.Type
    ( Wrapping(..)
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Language.GraphQL.AST.Document (Name)

-- | GraphQL distinguishes between "wrapping" and "named" types. Each wrapping
--   type can wrap other wrapping or named types. Wrapping types are lists and
--   Non-Null types (named types are nullable by default).
--
--   This 'Wrapping' type doesn\'t reflect this distinction exactly but it is
--   used in the resolvers to take into account that the returned value can be
--   nullable or an (arbitrary nested) list.
data Wrapping a
    = List [Wrapping a] -- ^ Arbitrary nested list
--    | Named a -- ^ Named type without further wrapping
    | Null -- ^ Null
    | O (HashMap Name a)
    | I Int
    | B Bool
    | F Float
    | E Text
    | S Text
    deriving (Eq, Show)

instance Functor Wrapping where
    fmap f (List list) = List $ fmap (fmap f) list
    fmap f (O map') = O $ f <$> map'
    fmap _ Null = Null
    fmap _ (I i) = I i
    fmap _ (B i) = B i
    fmap _ (F i) = F i
    fmap _ (E i) = E i
    fmap _ (S i) = S i

    {-instance Foldable Wrapping where
    foldr f acc (List list) = foldr (flip $ foldr f) acc list
    foldr f acc (O map') = foldr f acc map'
    foldr _ acc _ = acc -}

    {-instance Traversable Wrapping where
    traverse f (List list) = List <$> traverse (traverse f) list
    traverse f (Named named) = Named <$> f named
    traverse _ Null = pure Null
    traverse f (O map') = O <$> traverse f map'-}

{-instance Applicative Wrapping where
    pure = Named
    Null <*> _ = Null
    _ <*> Null = Null
    (Named f) <*> (Named x) = Named $ f x
    (List fs) <*> (List xs) = List $ (<*>) <$> fs <*> xs
    (Named f) <*> list = f <$> list
    (List fs) <*> named = List $ (<*> named) <$> fs

instance Monad Wrapping where
    return = pure
    Null >>= _ = Null
    (Named x) >>= f = f x
    (List xs) >>= f = List $ fmap (>>= f) xs-}
