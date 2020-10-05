{-# LANGUAGE CPP #-}
module Acc
(
  Acc,
  cons,
  snoc,
  uncons,
  unsnoc,
  toNonEmpty,
  enumFromTo,
)
where

import Acc.Prelude hiding (toNonEmpty, enumFromTo)
import qualified Acc.BinTree as BinTree
import qualified Data.Foldable as Foldable
import qualified Data.Semigroup.Foldable as Foldable1


{-|
Data structure intended for accumulating a sequence of elements
for later traversal or folding.
Useful for implementing all kinds of builders on top.

Appending and prepending is always \(\mathcal{O}(1)\).

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
  TreeAcc !(BinTree.BinTree a)
  deriving (Generic, Generic1)

instance NFData a => NFData (Acc a)

instance NFData1 Acc

deriving instance Functor Acc

instance Foldable Acc where
  foldMap f =
    \ case
      TreeAcc a ->
        foldMap f a
      EmptyAcc ->
        mempty
#if MIN_VERSION_base(4,13,0)
  foldMap' f =
    \ case
      TreeAcc a ->
        foldMap' f a
      EmptyAcc ->
        mempty
#endif
  foldr step acc =
    \ case
      TreeAcc a ->
        foldr step acc a
      EmptyAcc ->
        acc
  foldr' step acc =
    \ case
      TreeAcc a ->
        foldr' step acc a
      EmptyAcc ->
        acc
  foldl step acc =
    \ case
      TreeAcc a ->
        foldl step acc a
      EmptyAcc ->
        acc
  foldl' step acc =
    \ case
      TreeAcc a ->
        foldl' step acc a
      EmptyAcc ->
        acc
  sum =
    foldl' (+) 0

instance Traversable Acc where
  traverse f =
    \ case
      TreeAcc a ->
        TreeAcc <$> traverse f a
      EmptyAcc ->
        pure EmptyAcc

instance Applicative Acc where
  pure =
    TreeAcc . BinTree.Leaf
  (<*>) =
    \ case
      TreeAcc a ->
        \ case
          TreeAcc b ->
            TreeAcc (a <*> b)
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
            TreeAcc (BinTree.Branch a b)
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
      a : b -> TreeAcc (BinTree.fromList1 a b)
      _ -> EmptyAcc
  toList =
    \ case
      TreeAcc a ->
        foldr (:) [] a
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
      TreeAcc (BinTree.Branch (BinTree.Leaf a) tree)
    EmptyAcc ->
      TreeAcc (BinTree.Leaf a)

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
        BinTree.Branch l r ->
          case BinTree.unconsTo r l of
            (res, newTree) ->
              Just (res, TreeAcc newTree)
        BinTree.Leaf res ->
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
      TreeAcc (BinTree.Branch tree (BinTree.Leaf a))
    EmptyAcc ->
      TreeAcc (BinTree.Leaf a)

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
        BinTree.Branch l r ->
          case BinTree.unsnocTo l r of
            (res, newTree) ->
              Just (res, TreeAcc newTree)
        BinTree.Leaf res ->
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
      Just (Foldable1.toNonEmpty tree)
    EmptyAcc ->
      Nothing

{-|
Enumerate in range, inclusively.
-}
enumFromTo :: (Enum a, Ord a) => a -> a -> Acc a
enumFromTo from to =
  if from <= to
    then
      TreeAcc (BinTree.appendEnumFromTo (succ from) to (BinTree.Leaf from))
    else
      EmptyAcc
