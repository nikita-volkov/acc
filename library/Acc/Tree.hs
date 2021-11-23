module Acc.Tree where

import Acc.Prelude
import qualified Acc.Prelude as Prelude

data Tree a
  = -- | Branch with the smaller tree on the left.
    LeftBranch
      !Int
      !(Tree a)
      !(Tree a)
  | -- | Branch with the smaller tree on the right.
    RightBranch
      !Int
      !(Tree a)
      !(Tree a)
  | Leaf a

prepend :: Tree a -> Tree a -> Tree a
prepend = \case
  Leaf lleaf -> \case
    Leaf rleaf ->
      LeftBranch 2 (Leaf lleaf) (Leaf rleaf)
    LeftBranch size rlt rrt ->
      LeftBranch (succ size) (Leaf lleaf) (LeftBranch size rlt rrt)
    RightBranch size rlt rrt ->
      LeftBranch (succ size) (Leaf lleaf) (RightBranch size rlt rrt)
  LeftBranch lsize llt lrt -> \case
    LeftBranch rsize rlt rrt ->
      error "TODO"
