module Acc.Tree where

import Acc.Prelude
import qualified Acc.Prelude as Prelude

data Tree a
  = BranchTree !(Tree a) a !(Tree a)
  | LeafTree

fromList :: [a] -> Tree a
fromList = reduce LeafTree . reverse
  where
    reduce tree = \case
      a : b -> reduce (BranchTree LeafTree a tree) b
      [] -> tree
