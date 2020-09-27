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

All functions are implemented with tail recursion,
ensuring that you won\'t get stack overflow.
-}
data Acc a =
  EmptyAcc |
  TreeAcc !(BinTree1.BinTree1 a)

deriving instance Functor Acc

deriving instance Foldable Acc

deriving instance Traversable Acc

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
        Foldable.toList a
      _ ->
        []
