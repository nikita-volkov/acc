module Acc
(
  Acc,
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
  foldMap' f =
    \ case
      TreeAcc a ->
        BinTree1.foldMap' f a
      EmptyAcc ->
        mempty
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
