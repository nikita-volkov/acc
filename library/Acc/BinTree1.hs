module Acc.BinTree1
(
  BinTree1(..),
  foldM,
  ap,
  fromList1,
)
where

import Acc.Prelude hiding (foldM, ap)
import qualified Acc.Prelude as Prelude


data BinTree1 a =
  Leaf !a |
  Branch !(BinTree1 a) !(BinTree1 a)
  deriving (Generic, Generic1)

instance NFData a => NFData (BinTree1 a)

instance NFData1 BinTree1

deriving instance Functor BinTree1

instance Foldable BinTree1 where
  foldMap =
    foldMapDef
  foldMap' =
    foldMapDef'
  foldr =
    foldrDef
  foldl' =
    foldlDef'

foldM :: Monad m => (a -> b -> m a) -> a -> BinTree1 b -> m a
foldM step acc =
  \ case
    Branch a b -> foldMOnBranch step acc a b
    Leaf a -> step acc a

foldMOnBranch :: Monad m => (a -> b -> m a) -> a -> BinTree1 b -> BinTree1 b -> m a
foldMOnBranch step acc a b =
  case a of
    Leaf c -> step acc c >>= \ acc' -> foldM step acc' b
    Branch c d -> foldMOnBranch step acc c (Branch d b)

foldrDef :: (a -> b -> b) -> b -> BinTree1 a -> b
foldrDef step acc =
  \ case
    Branch a b -> foldrOnBranch step acc a b
    Leaf a -> step a acc

foldrOnBranch :: (a -> b -> b) -> b -> BinTree1 a -> BinTree1 a -> b
foldrOnBranch step acc a b =
  case a of
    Leaf c -> step c (foldrDef step acc b)
    Branch c d -> foldrOnBranch step acc c (Branch d b)

foldlDef' :: (b -> a -> b) -> b -> BinTree1 a -> b
foldlDef' step !acc =
  \ case
    Branch a b ->
      foldlOnBranch' step acc a b
    Leaf a ->
      step acc a

foldlOnBranch' :: (b -> a -> b) -> b -> BinTree1 a -> BinTree1 a -> b
foldlOnBranch' step acc a b =
  case a of
    Leaf c ->
      foldlDef' step (step acc c) b
    Branch c d ->
      foldlOnBranch' step acc c (Branch d b)

foldMapDef :: Monoid m => (a -> m) -> BinTree1 a -> m
foldMapDef =
  foldMapWithAcc mempty

foldMapWithAcc :: Monoid m => m -> (a -> m) -> BinTree1 a -> m
foldMapWithAcc acc map =
  \ case
    Branch a b -> foldMapOnBranch acc map a b
    Leaf a -> acc <> map a

foldMapOnBranch :: Monoid m => m -> (a -> m) -> BinTree1 a -> BinTree1 a -> m
foldMapOnBranch acc map a b =
  case a of
    Leaf c -> foldMapWithAcc (acc <> map c) map b
    Branch c d -> foldMapOnBranch acc map c (Branch d b)

foldMapDef' :: Monoid m => (a -> m) -> BinTree1 a -> m
foldMapDef' =
  foldMapWithAcc' mempty

foldMapWithAcc' :: Monoid m => m -> (a -> m) -> BinTree1 a -> m
foldMapWithAcc' !acc map =
  \ case
    Branch a b -> foldMapOnBranch' acc map a b
    Leaf a -> acc <> map a

foldMapOnBranch' :: Monoid m => m -> (a -> m) -> BinTree1 a -> BinTree1 a -> m
foldMapOnBranch' acc map a b =
  case a of
    Leaf c -> foldMapWithAcc' (acc <> map c) map b
    Branch c d -> foldMapOnBranch' acc map c (Branch d b)

instance Traversable BinTree1 where
  traverse map =
    \ case
      Branch a b ->
        traverseOnBranch map a b
      Leaf a ->
        Leaf <$> map a

traverseOnBranch :: Applicative f => (a -> f b) -> BinTree1 a -> BinTree1 a -> f (BinTree1 b)
traverseOnBranch map a b =
  case a of
    Leaf c ->
      Branch <$> Leaf <$> map c <*> traverse map b
    Branch c d ->
      traverseOnBranch map a (Branch d b)

ap :: BinTree1 (a -> b) -> BinTree1 a -> BinTree1 b
ap =
  \ case
    Branch a b ->
      \ c ->
        Branch (ap a c) (ap b c)
    Leaf a ->
      fmap a 

fromList1 :: a -> [a] -> BinTree1 a
fromList1 a =
  \ case
    b : c -> Branch (Leaf a) (fromList1 b c)
    _ -> Leaf a
