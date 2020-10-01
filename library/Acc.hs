{-# LANGUAGE CPP #-}
module Acc
(
  Acc,
  cons,
  snoc,
  uncons,
  unsnoc,
  toNonEmpty,
)
where

import Acc.Prelude
import qualified Acc.BinTree1 as BinTree1
import qualified Data.Foldable as Foldable


{-|
Data structure intended for accumulating a sequence of elements
for later traversal or folding.
Useful for implementing all kinds of builders on top.

To produce a single element 'Acc' use 'pure'.
To produce a multielement 'Acc' use 'fromList'.
To combine use '<|>' or '<>' and other 'Alternative' and 'Monoid'-related utils.
To extract elements use 'Foldable' API.

The benchmarks show that for the described use-case this data-structure
is on average 2 times faster than 'Data.DList.DList' and 'Data.Sequence.Seq',
is on par with list when you always prepend elements and
is exponentially faster than list when you append.

Internally it is implemented as a simple binary tree
with all functions optimized to use tail recursion,
ensuring that you don\'t get stack overflow.
-}
data Acc a =
  EmptyAcc |
  TreeAcc !(BinTree1.BinTree1 a)
  deriving (Generic, Generic1)

instance NFData a => NFData (Acc a)

instance NFData1 Acc

deriving instance Functor Acc

instance Foldable Acc where
  foldMap f =
    \ case
      TreeAcc a ->
        BinTree1.foldMap f a
      EmptyAcc ->
        mempty
#if MIN_VERSION_base(4,13,0)
  foldMap' f =
    \ case
      TreeAcc a ->
        BinTree1.foldMap' f a
      EmptyAcc ->
        mempty
#endif
  foldr step acc =
    \ case
      TreeAcc a ->
        BinTree1.foldr step acc a
      EmptyAcc ->
        acc
  foldr' step acc =
    \ case
      TreeAcc a ->
        BinTree1.foldr' step acc a
      EmptyAcc ->
        acc
  foldl step acc =
    \ case
      TreeAcc a ->
        BinTree1.foldl step acc a
      EmptyAcc ->
        acc
  foldl' step acc =
    \ case
      TreeAcc a ->
        BinTree1.foldl' step acc a
      EmptyAcc ->
        acc
  sum =
    foldl' (+) 0

instance Traversable Acc where
  traverse f =
    \ case
      TreeAcc a ->
        TreeAcc <$> BinTree1.traverse f a
      EmptyAcc ->
        pure EmptyAcc

instance Applicative Acc where
  pure =
    TreeAcc . BinTree1.Leaf
  (<*>) =
    \ case
      TreeAcc a ->
        \ case
          TreeAcc b ->
            TreeAcc (BinTree1.ap a b)
          EmptyAcc ->
            EmptyAcc
      EmptyAcc ->
        const EmptyAcc

instance Alternative Acc where
  empty =
    EmptyAcc
  (<|>) =
    \ case
      TreeAcc a ->
        \ case
          TreeAcc b ->
            TreeAcc (BinTree1.Branch a b)
          EmptyAcc ->
            TreeAcc a
      EmptyAcc ->
        id

instance Semigroup (Acc a) where
  (<>) =
    (<|>)

instance Monoid (Acc a) where
  mempty =
    empty
  mappend =
    (<>)

instance IsList (Acc a) where
  type Item (Acc a) = a
  fromList =
    \ case
      a : b -> TreeAcc (BinTree1.fromList1 a b)
      _ -> EmptyAcc
  toList =
    \ case
      TreeAcc a ->
        BinTree1.foldr (:) [] a
      _ ->
        []

instance Show a => Show (Acc a) where
  show =
    show . toList

{-|
Prepend an element.
-}
cons :: a -> Acc a -> Acc a
cons a =
  \ case
    TreeAcc tree ->
      TreeAcc (BinTree1.Branch (BinTree1.Leaf a) tree)
    EmptyAcc ->
      TreeAcc (BinTree1.Leaf a)

{-|
Extract the first element.

The produced accumulator will lack the extracted element
and will have the underlying tree rebalanced towards the beginning.
This means that calling 'uncons' on it will be \(\mathcal{O}(1)\) and
'unsnoc' will be \(\mathcal{O}(n)\).
-}
uncons :: Acc a -> Maybe (a, Acc a)
uncons =
  \ case
    TreeAcc tree ->
      case tree of
        BinTree1.Branch l r ->
          case BinTree1.unconsTo r l of
            (res, newTree) ->
              Just (res, TreeAcc newTree)
        BinTree1.Leaf res ->
          Just (res, EmptyAcc)
    EmptyAcc ->
      Nothing

{-|
Append an element.
-}
snoc :: a -> Acc a -> Acc a
snoc a =
  \ case
    TreeAcc tree ->
      TreeAcc (BinTree1.Branch tree (BinTree1.Leaf a))
    EmptyAcc ->
      TreeAcc (BinTree1.Leaf a)

{-|
Extract the last element.

The produced accumulator will lack the extracted element
and will have the underlying tree rebalanced towards the end.
This means that calling 'unsnoc' on it will be \(\mathcal{O}(1)\) and
'uncons' will be \(\mathcal{O}(n)\).
-}
unsnoc :: Acc a -> Maybe (a, Acc a)
unsnoc =
  \ case
    TreeAcc tree ->
      case tree of
        BinTree1.Branch l r ->
          case BinTree1.unsnocTo l r of
            (res, newTree) ->
              Just (res, TreeAcc newTree)
        BinTree1.Leaf res ->
          Just (res, EmptyAcc)
    EmptyAcc ->
      Nothing

{-|
Convert to non empty list if it's not empty.
-}
toNonEmpty :: Acc a -> Maybe (NonEmpty a)
toNonEmpty =
  \ case
    TreeAcc tree ->
      Just (BinTree1.toNonEmpty tree)
    EmptyAcc ->
      Nothing
