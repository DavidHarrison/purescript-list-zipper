module Data.List.Zipper
    ( Zipper(..)
    , up, down
    , beginning, end
    , fromFoldable, toUnfoldable
    ) where

import Prelude

import Control.Comonad      (Comonad, extract)
import Control.Extend       (Extend, extend)
import Data.List            (List(..), head, reverse, snoc, tail, toList)
import Data.Foldable        (Foldable, foldl, foldMap, foldr, intercalate)
import Data.Functor.Compose (Compose(..))
import Data.Maybe           (Maybe(..), maybe)
import Data.Traversable     (Traversable, sequence, traverse)
import Data.Tuple           (Tuple(Tuple))
import Data.Unfoldable      (Unfoldable, unfoldr)

data Zipper a = Zipper (List a) a (List a)

-- | O(1) Move one step closer to the start of the Zipper.
-- | This is the inverse of `down` where their composition is defined
-- | (while not at one of the ends of the Zipper).
up :: forall a. Zipper a -> Maybe (Zipper a)
up (Zipper ls c rs) = Zipper <$> ls' <*> c' <*> rs'
  where ls' = tail ls
        c'  = head ls
        rs' = pure (Cons c rs)

-- | O(1) Move one step closer to the end of the Zipper.
-- | This is the inverse of `up` where their composition is defined
-- | (while not at one of the ends of the Zipper).
down :: forall a. Zipper a -> Maybe (Zipper a)
down (Zipper ls c rs) = Zipper <$> ls' <*> c' <*> rs'
  where ls' = pure (Cons c ls)
        c'  = head rs
        rs' = tail rs

-- | O(n) Go to the beginning of the Zipper.
-- | This should be an idempotent operation of the Zipper (once at the
-- | beginning, moving to the head will not change the focus).
beginning :: forall a. Zipper a -> Zipper a
beginning = lastJust up

-- | O(n) Go to the end of the Zipper.
-- | This should be an idempotent operation of the Zipper (once at the end,
-- | moving to the head will not change the focus).
end :: forall a. Zipper a -> Zipper a
end = lastJust down

-- | Convert a Zipper into any type with an Unfoldable instance. Assuming
-- | that the definition of `unfoldr` is O(n), the unfolding will be
-- | O(2 * l + r) where l is the length of the lefthand/top list of the Zipper
-- | and r is the length of the righthand/bottom.
toUnfoldable :: forall a f. (Unfoldable f) => Zipper a -> f a
toUnfoldable (Zipper ls c rs) = unfoldr iter $ Tuple (reverse ls) (Cons c rs)
  where iter :: (Tuple (List a) (List a)) -> Maybe (Tuple a (Tuple (List a) (List a)))
        iter (Tuple (Cons a as) bs)  = Just $ Tuple a $ Tuple as bs
        iter (Tuple Nil (Cons b bs)) = Just $ Tuple b $ Tuple Nil bs
        iter (Tuple Nil Nil)         = Nothing

-- | Convert any type with a Foldable instance into a Zipper with the
-- | possibility for failure (in the case of an empty Foldable). Assuming the
-- | definition of `foldr` is O(n), the folding will also be O(n).
fromFoldable :: forall a f. (Foldable f) => f a -> Maybe (Zipper a)
fromFoldable x = Zipper Nil <$> head asList <*> tail asList
  where asList = toList x

-------------------------------------------------------------------------------
-- Instances ------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Show the Zipper as a list with the focus in curly-braces ({}).
instance showZipper :: (Show a) => Show (Zipper a) where
    -- show :: forall a. (Show a) => Zipper a -> String
    show (Zipper ls c rs) = "[" <> intercalate ", " (ls' <> Cons c' rs') <> "]"
      where ls' = map show $ reverse ls
            c'  = "{" <> show c <> "}"
            rs' = map show rs

-- | Check the equality of two Zippers.
-- | NOTE: the focus is part of the equality.
instance eqZipper :: (Eq a) => Eq (Zipper a) where
    -- eq :: forall a. (Eq a) => Zipper a -> Zipper a -> Bool
    eq (Zipper ls c rs) (Zipper ls' c' rs') =
        ls == ls' && c == c' && rs == rs'

{- TODO: Comparison is complicated by the position of the cursor, I am not sure
   what the correct implementation is (although the following seems to be
   law abiding), so I will leave it commented for now.
-- | Compare two Zippers starting from their left-/top-most element.
instance ordZipper :: (Ord a) => Ord (Zipper a) where
    -- compare :: forall a. (Ord a) => Zipper a -> Zipper a -> Ordering
    compare (Zipper ls c rs) (Zipper ls' c' rs') =
        compare (reverse ls) (reverse ls') <> compare c c' <> compare rs rs'
-}

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

instance unfoldableMaybeZipper :: Unfoldable (Compose Maybe Zipper) where
    unfoldr f x = Compose $ Zipper Nil <$> head asList <*> tail asList
      where asList = unfoldr f x

-------------------------------------------------------------------------------
-- Helper Functions -----------------------------------------------------------
-------------------------------------------------------------------------------

-- Iteratively applies the provided function until it reaches a `Nothing` and
-- returns the last `Just` value.
lastJust :: forall a. (a -> Maybe a) -> a -> a
lastJust f x = maybe x (lastJust f) $ f x

maybeIterate :: forall a f. (Unfoldable f) => (a -> Maybe a) -> a -> f a
maybeIterate f = unfoldr (map dup <<< f)
  where dup a = Tuple a a
