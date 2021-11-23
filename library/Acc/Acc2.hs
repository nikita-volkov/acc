module Acc.Acc2 where

import Acc.Prelude
import qualified Acc.Prelude as Prelude

data Acc a
  = TreeAcc
      !Int
      -- ^ Size.
      !(Tree a)
  | EmptyAcc

data Tree a
  = -- | Branch with the smaller tree on the left.
    LeftBranchTree
      !(Tree a)
      !(Tree a)
  | -- | Branch with the smaller tree on the right.
    RightBranchTree
      !(Tree a)
      !(Tree a)
  | LeafTree a

instance Semigroup (Acc a) where
  (<>) = \case
    TreeAcc ls lt -> \case
      TreeAcc rs rt ->
        TreeAcc (ls + rs) $
          if ls > rs
            then RightBranchTree lt rt
            else LeftBranchTree lt rt
      EmptyAcc ->
        TreeAcc ls lt
    EmptyAcc -> id
