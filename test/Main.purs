module Test.Main where

import Prelude

import Control.Bind                             ((<=<))
import Control.Monad.Eff.Console                (log)
import Data.Foldable                            (Foldable, foldl)
import Data.List                                (List(), fromList, toList)
import Data.Maybe                               (Maybe(..), maybe)
import Test.QuickCheck                          ( Result(Success)
                                                , (===)
                                                , quickCheck
                                                )
import Test.QuickCheck.Arbitrary                ( Arbitrary
                                                , Coarbitrary
                                                , arbitrary
                                                , coarbitrary
                                                )
import Test.QuickCheck.Gen                      (Gen())
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Apply       (checkApply)
import Test.QuickCheck.Laws.Control.Comonad     (checkComonad)
import Test.QuickCheck.Laws.Control.Extend      (checkExtend)
import Test.QuickCheck.Laws.Data.Eq             (checkEq)
import Test.QuickCheck.Laws.Data.Functor        (checkFunctor)
import Test.QuickCheck.Laws.Data.Ord            (checkOrd)
import Type.Proxy                               (Proxy(Proxy), Proxy2(Proxy2))

import Data.List.Zipper ( Zipper(..)
                        , up, down
                        , beginning, end
                        , fromFoldable, toUnfoldable
                        )

main = do
    log "Checking that `show` is total"
    quickCheck $ const Success $ show :: Zipper Number -> String
    checkEq      prxZipper
    checkOrd     prxZipper
    checkFunctor prxZipper2 prxA prxB
    checkExtend  prxZipper2 prxA prxB prxC
    checkComonad prxZipper2 prxA prxB
    -- TODO: check Foldable instance
    -- TODO: Uncomment when Traversable laws are merged into purescript-quickcheck-laws
    -- checkTraversable prxZipper2 prxF2 prxG2 prxA prxB prxC
    log "Checking that composition of fromFoldable and toUnfoldable preserve elements and order"
    quickCheck $ notUnequal (Just <<< beginning) $ map beginning <<< fromFoldable <<< (toUnfoldable :: Zipper Number -> Array Number)
    -- TODO: assure that we exclude necessary information loss
    -- (as with beginning above) in the other Foldable/Unfoldable
    -- (does not matter for Array)
    quickCheck $ notUnequal Just $ return <<< toUnfoldable <=< (fromFoldable :: Array Number -> Maybe (Zipper Number))
    log "Checking that `up` and `down` are inverses where their composition is defined"
    quickCheck $ notUnequal Just $ (up <=< down) :: Zipper Number -> Maybe (Zipper Number)
    quickCheck $ notUnequal Just $ (down <=< up) :: Zipper Number -> Maybe (Zipper Number)
    log "Checking that `beginning` and `end` are idempotent"
    quickCheck $ idempotent $ beginning :: Zipper Number -> Zipper Number
    quickCheck $ idempotent $ end       :: Zipper Number -> Zipper Number

prxZipper2 :: Proxy2 Zipper
prxZipper2 =  Proxy2
-- prxF2 :: Proxy2 Array
-- prxF2 = Proxy2
-- prxG2 :: Proxy2 Maybe
-- prxG2 = Proxy2
prxZipper :: Proxy (Zipper Number)
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

instance arbList :: (Arbitrary a) => Arbitrary (List a) where
    -- arbitrary :: forall a. (Arbitrary a) => Gen (List a)
    arbitrary = toList <$> (arbitrary :: Gen (Array a))

instance coarbFoldable :: (Foldable f, Coarbitrary a) => Coarbitrary (f a) where
    -- coarbitrary :: forall a r. (Foldable f, Coarbitrary a) => (f a) -> Gen r -> Gen r
    coarbitrary = foldl (\f x -> f <<< coarbitrary x) id

instance arbZipper :: (Arbitrary a) => Arbitrary (Zipper a) where
    -- arbitrary :: forall a. (Arbitrary a) => Gen (Zipper a)
    arbitrary = Zipper <$> arbitrary <*> arbitrary <*> arbitrary
