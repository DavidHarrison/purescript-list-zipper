module Data.List.Zipper
    ( Zipper(..)
    , up, down
    , beginning, end
    , fromFoldable, toUnfoldable
    ) where

import Prelude

import Control.Comonad  (Comonad, extract)
import Control.Extend   (Extend, extend)
import Data.List        (List(..), head, reverse, snoc, tail, toList)
import Data.Foldable    (Foldable, foldl, foldMap, foldr, intercalate)
import Data.Maybe       (Maybe(..), maybe)
import Data.Traversable (Traversable, sequence, traverse)
import Data.Tuple       (Tuple(Tuple))
import Data.Unfoldable  (Unfoldable, unfoldr)

data Zipper a = Zipper (List a) a (List a)

up :: forall a. Zipper a -> Maybe (Zipper a)
up (Zipper ls c rs) = Zipper <$> ls' <*> c' <*> rs'
  where ls' = tail ls
        c'  = head ls
        rs' = pure (Cons c rs)

down :: forall a. Zipper a -> Maybe (Zipper a)
down (Zipper ls c rs) = Zipper <$> ls' <*> c' <*> rs'
  where ls' = pure (Cons c ls)
        c'  = head rs
        rs' = tail rs

beginning :: forall a. Zipper a -> Zipper a
beginning = lastJust up

end :: forall a. Zipper a -> Zipper a
end = lastJust down

toUnfoldable :: forall a f. (Unfoldable f) => Zipper a -> f a
toUnfoldable (Zipper ls c rs) = unfoldr iter $ Tuple (reverse ls) (Cons c rs)
  where iter :: (Tuple (List a) (List a)) -> Maybe (Tuple a (Tuple (List a) (List a)))
        iter (Tuple (Cons a as) bs)  = Just $ Tuple a $ Tuple as bs
        iter (Tuple Nil (Cons b bs)) = Just $ Tuple b $ Tuple Nil bs
        iter (Tuple Nil Nil)         = Nothing

fromFoldable :: forall a f. (Foldable f) => f a -> Maybe (Zipper a)
fromFoldable x = Zipper Nil <$> head asList <*> tail asList
  where asList = toList x

-------------------------------------------------------------------------------
-- Instances ------------------------------------------------------------------
-------------------------------------------------------------------------------

-- show the zipper with the focus in curly-braces ({})
instance showZipper :: (Show a) => Show (Zipper a) where
    -- show :: forall a. (Show a) => Zipper a -> String
    show (Zipper ls c rs) = "[" <> intercalate ", " (ls' <> Cons c' rs') <> "]"
      where ls' = map show $ reverse ls
            c'  = "{" <> show c <> "}"
            rs' = map show rs

-- NOTE: the focus is part of the equality for the Zipper
instance eqZipper :: (Eq a) => Eq (Zipper a) where
    -- eq :: forall a. (Eq a) => Zipper a -> Zipper a -> Bool
    eq (Zipper ls c rs) (Zipper ls' c' rs') =
        ls == ls' && c == c' && rs == rs'

instance ordZipper :: (Ord a) => Ord (Zipper a) where
    -- compare :: forall a. (Ord a) => Zipper a -> Zipper a -> Ordering
    compare (Zipper ls c rs) (Zipper ls' c' rs') =
        compare ls ls' <> compare c c' <> compare rs rs'

instance functorZipper :: Functor Zipper where
    -- map :: forall a b. (a -> b) -> Zipper a -> Zipper b
    map f (Zipper ls c rs) = Zipper (f <$> ls) (f c) (f <$> rs)

instance extendZipper :: Extend Zipper where
    -- extend :: forall b a. (Zipper a -> b) -> Zipper a -> Zipper b
    extend f = Zipper <$> go f up <*> f <*> go f down
        where go f d = (<$>) f <<< maybeIterate d

instance comonadZipper :: Comonad Zipper where
    -- extract :: forall a. Zipper a -> a
    extract (Zipper _ c _) = c

instance foldableZipper :: Foldable Zipper where
    -- foldr :: forall a b. (a -> b -> b) -> b -> (Zipper a) -> b
    foldr f x (Zipper ls c rs) = flip (foldr f) (reverse ls)
                               $ f c
                               $ foldr f x rs
    -- foldl :: forall a b. (b -> a -> b) -> b -> (Zipper a) -> b
    foldl f x (Zipper ls c rs) = flip (foldl f) rs
                               $ flip f c
                               $ foldl f x (reverse ls)
    -- foldMap :: forall a m. (Monoid m) => (a -> m) -> (Zipper a) -> m
    foldMap f (Zipper ls c rs) = foldMap f (reverse ls) <> f c <> foldMap f rs

instance traversableZipper :: Traversable Zipper where
    -- traverse :: forall a b m. (Applicative m) => (a -> m b) -> (Zipper a) -> m (Zipper b)
    traverse f (Zipper ls c rs) = Zipper <$> traverse f (reverse ls)
                                         <*> f c
                                         <*> traverse f rs
    -- sequence :: forall a m. (Applicative m) => (Zipper (m a)) -> m (Zipper a)
    sequence = traverse id

-------------------------------------------------------------------------------
-- Helper Functions -----------------------------------------------------------
-------------------------------------------------------------------------------
lastJust :: forall a. (a -> Maybe a) -> a -> a
lastJust f x = maybe x (lastJust f) $ f x

maybeIterate :: forall a f. (Unfoldable f) => (a -> Maybe a) -> a -> f a
maybeIterate f = unfoldr (map dup <<< f)
  where dup a = Tuple a a
