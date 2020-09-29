module Acc.BinTree1
(
  BinTree1(..),
  foldM,
  ap,
  fromList1,
  foldMap,
  foldMap',
  foldr,
  foldr',
  foldl,
  foldl',
  traverse,
  uncons,
  unconsTo,
  unsnoc,
  unsnocTo,
)
where

import Acc.Prelude hiding (ap, foldM, foldl, foldl', foldr, foldr', foldMap, foldMap', traverse)
import qualified Acc.Prelude as Prelude


data BinTree1 a =
  Leaf !a |
  Branch !(BinTree1 a) !(BinTree1 a)
  deriving (Generic, Generic1, Show)

instance NFData a => NFData (BinTree1 a)

instance NFData1 BinTree1

deriving instance Functor BinTree1

foldM :: Monad m => (a -> b -> m a) -> a -> BinTree1 b -> m a
foldM step !acc =
  \ case
    Branch a b -> foldMOnBranch step acc a b
    Leaf a -> step acc a

foldMOnBranch :: Monad m => (a -> b -> m a) -> a -> BinTree1 b -> BinTree1 b -> m a
foldMOnBranch step acc a b =
  case a of
    Leaf c -> step acc c >>= \ acc' -> foldM step acc' b
    Branch c d -> foldMOnBranch step acc c (Branch d b)

foldr :: (a -> b -> b) -> b -> BinTree1 a -> b
foldr step acc =
  \ case
    Branch a b ->
      foldrOnBranch step acc a b
    Leaf a ->
      step a acc

foldrOnBranch :: (a -> b -> b) -> b -> BinTree1 a -> BinTree1 a -> b
foldrOnBranch step acc a b =
  case a of
    Leaf c ->
      step c (foldr step acc b)
    Branch c d ->
      foldrOnBranch step acc c (Branch d b)

foldr' :: (a -> b -> b) -> b -> BinTree1 a -> b
foldr' step !acc =
  \ case
    Branch a b -> foldrOnBranch' step acc a b
    Leaf a -> step a acc

foldrOnBranch' :: (a -> b -> b) -> b -> BinTree1 a -> BinTree1 a -> b
foldrOnBranch' step acc a b =
  case b of
    Leaf c -> foldr' step (step c acc) a
    Branch c d -> foldrOnBranch' step acc (Branch a c) d

foldl :: (b -> a -> b) -> b -> BinTree1 a -> b
foldl step acc =
  \ case
    Branch a b ->
      foldlOnBranch step acc a b
    Leaf a ->
      step acc a

foldlOnBranch :: (b -> a -> b) -> b -> BinTree1 a -> BinTree1 a -> b
foldlOnBranch step acc a b =
  case b of
    Leaf c ->
      step (foldl step acc a) c
    Branch c d ->
      foldlOnBranch step acc (Branch a c) d

foldl' :: (b -> a -> b) -> b -> BinTree1 a -> b
foldl' step !acc =
  \ case
    Branch a b ->
      foldlOnBranch' step acc a b
    Leaf a ->
      step acc a

foldlOnBranch' :: (b -> a -> b) -> b -> BinTree1 a -> BinTree1 a -> b
foldlOnBranch' step acc a b =
  case a of
    Leaf c ->
      foldl' step (step acc c) b
    Branch c d ->
      foldlOnBranch' step acc c (Branch d b)

foldMap :: Monoid m => (a -> m) -> BinTree1 a -> m
foldMap =
  foldMapTo mempty

foldMapTo :: Monoid m => m -> (a -> m) -> BinTree1 a -> m
foldMapTo acc map =
  \ case
    Branch a b -> foldMapToOnBranch acc map a b
    Leaf a -> acc <> map a

foldMapToOnBranch :: Monoid m => m -> (a -> m) -> BinTree1 a -> BinTree1 a -> m
foldMapToOnBranch acc map a b =
  case a of
    Leaf c -> foldMapTo (acc <> map c) map b
    Branch c d -> foldMapToOnBranch acc map c (Branch d b)

foldMap' :: Monoid m => (a -> m) -> BinTree1 a -> m
foldMap' =
  foldMapTo' mempty

foldMapTo' :: Monoid m => m -> (a -> m) -> BinTree1 a -> m
foldMapTo' !acc map =
  \ case
    Branch a b -> foldMapToOnBranch' acc map a b
    Leaf a -> acc <> map a

foldMapToOnBranch' :: Monoid m => m -> (a -> m) -> BinTree1 a -> BinTree1 a -> m
foldMapToOnBranch' acc map a b =
  case a of
    Leaf c -> foldMapTo' (acc <> map c) map b
    Branch c d -> foldMapToOnBranch' acc map c (Branch d b)

traverse :: Applicative f => (a -> f b) -> BinTree1 a -> f (BinTree1 b)
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
    b : c -> fromList1To (Leaf a) b c
    _ -> Leaf a

fromList1To :: BinTree1 a -> a -> [a] -> BinTree1 a
fromList1To leftTree a =
  \ case
    b : c -> fromList1To (Branch leftTree (Leaf a)) b c
    _ -> Branch leftTree (Leaf a)

uncons :: BinTree1 a -> (a, Maybe (BinTree1 a))
uncons =
  \ case
    Branch l r ->
      fmap Just (unconsTo r l)
    Leaf a ->
      (a, Nothing)

unconsTo :: BinTree1 a -> BinTree1 a -> (a, BinTree1 a)
unconsTo buff =
  \ case
    Branch l r ->
      unconsTo (Branch r buff) l
    Leaf a ->
      (a, buff)

unsnoc :: BinTree1 a -> (a, Maybe (BinTree1 a))
unsnoc =
  \ case
    Branch l r ->
      fmap Just (unsnocTo l r)
    Leaf a ->
      (a, Nothing)

unsnocTo :: BinTree1 a -> BinTree1 a -> (a, BinTree1 a)
unsnocTo buff =
  \ case
    Branch l r ->
      unsnocTo (Branch l buff) r
    Leaf a ->
      (a, buff)
