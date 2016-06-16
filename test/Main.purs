module Test.Main where

import Prelude

import Control.Bind                             ((<=<))
import Control.Monad.Eff                        (Eff())
import Control.Monad.Eff.Console                (CONSOLE(), log)
import Control.Monad.Eff.Random                 (RANDOM())
import Control.Monad.Eff.Exception              (EXCEPTION())
import Data.Foldable                            ( class Foldable
                                                , foldl
                                                , foldMap
                                                , foldr
                                                )
import Data.Maybe                               (Maybe(..), maybe)
import Test.QuickCheck                          ( Result(Success)
                                                , (===)
                                                , quickCheck
                                                )
import Test.QuickCheck.Arbitrary                ( class Arbitrary
                                                , class Coarbitrary
                                                , arbitrary
                                                , coarbitrary
                                                )
import Test.QuickCheck.Laws.Control.Comonad     (checkComonad)
import Test.QuickCheck.Laws.Control.Extend      (checkExtend)
import Test.QuickCheck.Laws.Data.Eq             (checkEq)
import Test.QuickCheck.Laws.Data.Functor        (checkFunctor)
-- import Test.QuickCheck.Laws.Data.Ord            (checkOrd)
import Type.Proxy                               (Proxy(Proxy), Proxy2(Proxy2))

import Data.List.Zipper (Zipper(..))

-- Qualified, since we'll need to wrap them for ArbitraryZipper
import Data.List.Zipper
    ( up, down
    , beginning, end
    , fromFoldable, toUnfoldable
    ) as Zipper

import Control.Extend (class Extend, extend)
import Control.Comonad (class Comonad, extract)
import Data.Unfoldable (class Unfoldable)


main :: forall eff. Eff
        ( console :: CONSOLE
        , random :: RANDOM
        , err :: EXCEPTION
        | eff)
        Unit
main = do
    log "Checking that `show` is total"
    quickCheck $ const Success $ show :: ArbitraryZipper Number -> String

    checkEq      prxZipper
    -- checkOrd     prxZipper
    checkFunctor prxZipper2
    checkExtend  prxZipper2
    checkComonad prxZipper2

    log "Checking that folding is preserved by natural transformations to an element and order preserving structure"
    quickCheck $ \f -> (===) <$> foldMap (f :: Int -> String) <*> foldMap f <<< (toUnfoldable :: ArbitraryZipper Int -> Array Int)
    quickCheck $ \f i -> (===) <$> foldr (f :: Int -> String -> String) i <*> foldr f i <<< (toUnfoldable :: ArbitraryZipper Int -> Array Int)
    quickCheck $ \f i -> (===) <$> foldl (f :: String -> Int -> String) i <*> foldl f i <<< (toUnfoldable :: ArbitraryZipper Int -> Array Int)

    -- TODO: Uncomment when Traversable laws are merged into
    --       purescript-quickcheck-laws
    -- checkTraversable prxZipper2 prxF2 prxG2 prxA prxB prxC
    log "Checking that composition of fromFoldable and toUnfoldable preserve elements and order"
    quickCheck $ notUnequal (Just <<< beginning)
               $ map beginning <<< fromFoldable
                               <<< (toUnfoldable :: ArbitraryZipper Number -> Array Number)
    -- TODO: assure that we exclude information loss (as with beginning above)
    --       in the other Foldable/Unfoldable (does not matter for Array)
    quickCheck $ notUnequal Just
               $ pure <<< (toUnfoldable :: ArbitraryZipper Number -> Array Number)
                        <=< fromFoldable
    log "Checking that `up` and `down` are inverses where their composition is defined"
    quickCheck $ notUnequal Just $ (up <=< down) :: ArbitraryZipper Number -> Maybe (ArbitraryZipper Number)
    quickCheck $ notUnequal Just $ (down <=< up) :: ArbitraryZipper Number -> Maybe (ArbitraryZipper Number)
    log "Checking that `beginning` and `end` are idempotent"
    quickCheck $ idempotent $ beginning :: ArbitraryZipper Number -> ArbitraryZipper Number
    quickCheck $ idempotent $ end       :: ArbitraryZipper Number -> ArbitraryZipper Number


prxZipper2 :: Proxy2 ArbitraryZipper
prxZipper2 =  Proxy2
-- prxF2 :: Proxy2 Array
-- prxF2 = Proxy2
-- prxG2 :: Proxy2 Maybe
-- prxG2 = Proxy2
prxZipper :: Proxy (ArbitraryZipper Number)
prxZipper = Proxy
prxA :: Proxy Boolean
prxA = Proxy
prxB :: Proxy String
prxB = Proxy
prxC :: Proxy Int
prxC = Proxy

notUnequal :: forall a. (Eq a, Show a) => (a -> Maybe a) -> (a -> Maybe a) -> a -> Result
notUnequal f g x = maybe Success id $ (===) <$> f x <*> g x

idempotent :: forall a. (Eq a) => (a -> a) -> a -> Boolean
idempotent f = eq <$> f <<< f <*> f

-- We can't make an arbitrary instance here for Zipper itself, because of orphan instances.
-- We could make one in the main module, but then we'd pull in Quickcheck in production code,
-- which isn't really ideal. So, we actually test a newtype, and make the necessary instances
-- here, so that we're really testing Zipper.
newtype ArbitraryZipper a = ArbitraryZipper (Zipper a)

instance arbArbitraryZipper :: (Arbitrary a) => Arbitrary (ArbitraryZipper a) where
    -- arbitrary :: forall a. (Arbitrary a) => Gen (Zipper a)
    arbitrary = ArbitraryZipper <$> (Zipper <$> arbitrary <*> arbitrary <*> arbitrary)

instance coarbArbitraryZipper :: (Coarbitrary a) => Coarbitrary (ArbitraryZipper a) where
    coarbitrary (ArbitraryZipper (Zipper left middle right)) = coarbitrary left >>> coarbitrary middle >>> coarbitrary right

-- Wrapping Zipper functions for ArbitraryZipper
up :: forall a. ArbitraryZipper a -> Maybe (ArbitraryZipper a)
up (ArbitraryZipper zipper) = ArbitraryZipper <$> Zipper.up zipper

down :: forall a. ArbitraryZipper a -> Maybe (ArbitraryZipper a)
down (ArbitraryZipper zipper) = ArbitraryZipper <$> Zipper.down zipper

beginning :: forall a. ArbitraryZipper a -> ArbitraryZipper a
beginning (ArbitraryZipper zipper) = ArbitraryZipper $ Zipper.beginning zipper

end :: forall a. ArbitraryZipper a -> ArbitraryZipper a
end (ArbitraryZipper zipper) = ArbitraryZipper $ Zipper.end zipper

toUnfoldable :: forall a f. (Unfoldable f) => ArbitraryZipper a -> f a
toUnfoldable (ArbitraryZipper zipper) = Zipper.toUnfoldable zipper

fromFoldable :: forall a f. (Foldable f) => f a -> Maybe (ArbitraryZipper a)
fromFoldable f = ArbitraryZipper <$> Zipper.fromFoldable f

-- Purescript will someday be able to derive most of the rest of these ...
-- see https://github.com/purescript/purescript/issues/514.
instance showArbitraryZipper :: (Show a) => Show (ArbitraryZipper a) where
    show (ArbitraryZipper zipper) = show zipper

instance eqArbitraryZipper :: (Eq a) => Eq (ArbitraryZipper a) where
    eq (ArbitraryZipper zipper1) (ArbitraryZipper zipper2) = eq zipper1 zipper2

instance functorArbitraryZipper :: Functor ArbitraryZipper where
    map func (ArbitraryZipper zipper) = ArbitraryZipper $ map func zipper

instance extendArbitraryZipper :: Extend ArbitraryZipper where
    extend func (ArbitraryZipper zipper) = ArbitraryZipper $ extend (func <<< ArbitraryZipper) zipper

instance comonadArbitraryZipper :: Comonad ArbitraryZipper where
    extract (ArbitraryZipper zipper) = extract zipper

instance foldableArbitraryZipper :: Foldable ArbitraryZipper where
    foldr func memo (ArbitraryZipper zipper) = foldr func memo zipper
    foldl func memo (ArbitraryZipper zipper) = foldl func memo zipper
    foldMap func (ArbitraryZipper zipper) = foldMap func zipper
