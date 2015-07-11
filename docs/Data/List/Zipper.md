## Module Data.List.Zipper

#### `Zipper`

``` purescript
data Zipper a
  = Zipper (List a) a (List a)
```

##### Instances
``` purescript
instance showZipper :: (Show a) => Show (Zipper a)
instance eqZipper :: (Eq a) => Eq (Zipper a)
instance functorZipper :: Functor Zipper
instance extendZipper :: Extend Zipper
instance comonadZipper :: Comonad Zipper
instance foldableZipper :: Foldable Zipper
instance traversableZipper :: Traversable Zipper
instance unfoldableMaybeZipper :: Unfoldable (Compose Maybe Zipper)
```

#### `up`

``` purescript
up :: forall a. Zipper a -> Maybe (Zipper a)
```

O(1) Move one step closer to the start of the Zipper.
This is the inverse of `down` where their composition is defined
(while not at one of the ends of the Zipper).

#### `down`

``` purescript
down :: forall a. Zipper a -> Maybe (Zipper a)
```

O(1) Move one step closer to the end of the Zipper.
This is the inverse of `up` where their composition is defined
(while not at one of the ends of the Zipper).

#### `beginning`

``` purescript
beginning :: forall a. Zipper a -> Zipper a
```

O(n) Go to the beginning of the Zipper.
This should be an idempotent operation of the Zipper (once at the
beginning, moving to the head will not change the focus).

#### `end`

``` purescript
end :: forall a. Zipper a -> Zipper a
```

O(n) Go to the end of the Zipper.
This should be an idempotent operation of the Zipper (once at the end,
moving to the head will not change the focus).

#### `toUnfoldable`

``` purescript
toUnfoldable :: forall a f. (Unfoldable f) => Zipper a -> f a
```

Convert a Zipper into any type with an Unfoldable instance. Assuming
that the definition of `unfoldr` is O(n), the unfolding will be
O(2 * l + r) where l is the length of the lefthand/top list of the Zipper
and r is the length of the righthand/bottom.

#### `fromFoldable`

``` purescript
fromFoldable :: forall a f. (Foldable f) => f a -> Maybe (Zipper a)
```

Convert any type with a Foldable instance into a Zipper with the
possibility for failure (in the case of an empty Foldable). Assuming the
definition of `foldr` is O(n), the folding will also be O(n).


