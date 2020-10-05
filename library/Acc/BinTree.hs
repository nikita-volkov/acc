{-# LANGUAGE CPP #-}
module Acc.BinTree
(
  BinTree(..),
  foldM,
  fromList1,
  uncons,
  unconsTo,
  unsnoc,
  unsnocTo,
  appendEnumFromTo,
)
where

import Acc.Prelude hiding (foldM)
import qualified Acc.Prelude as Prelude


data BinTree a =
  Leaf !a |
  Branch !(BinTree a) !(BinTree a)
  deriving (Generic, Generic1, Show)

instance NFData a => NFData (BinTree a)

instance NFData1 BinTree

deriving instance Functor BinTree

instance Applicative BinTree where
  pure =
    Leaf
  (<*>) =
    \ case
      Branch a b ->
        \ c ->
          Branch (a <*> c) (b <*> c)
      Leaf a ->
        fmap a 

instance Foldable BinTree where
  
  foldr :: (a -> b -> b) -> b -> BinTree a -> b
  foldr step acc =
    \ case
      Branch a b ->
        foldrOnBranch step acc a b
      Leaf a ->
        step a acc
    where
      foldrOnBranch :: (a -> b -> b) -> b -> BinTree a -> BinTree a -> b
      foldrOnBranch step acc a b =
        case a of
          Leaf c ->
            step c (foldr step acc b)
          Branch c d ->
            foldrOnBranch step acc c (Branch d b)

  foldr' :: (a -> b -> b) -> b -> BinTree a -> b
  foldr' step !acc =
    \ case
      Branch a b -> foldrOnBranch' step acc a b
      Leaf a -> step a acc
    where
      foldrOnBranch' :: (a -> b -> b) -> b -> BinTree a -> BinTree a -> b
      foldrOnBranch' step acc a b =
        case b of
          Leaf c -> foldr' step (step c acc) a
          Branch c d -> foldrOnBranch' step acc (Branch a c) d

  foldl :: (b -> a -> b) -> b -> BinTree a -> b
  foldl step acc =
    \ case
      Branch a b ->
        foldlOnBranch step acc a b
      Leaf a ->
        step acc a
    where
      foldlOnBranch :: (b -> a -> b) -> b -> BinTree a -> BinTree a -> b
      foldlOnBranch step acc a b =
        case b of
          Leaf c ->
            step (foldl step acc a) c
          Branch c d ->
            foldlOnBranch step acc (Branch a c) d

  foldl' :: (b -> a -> b) -> b -> BinTree a -> b
  foldl' step !acc =
    \ case
      Branch a b ->
        foldlOnBranch' step acc a b
      Leaf a ->
        step acc a
    where
      foldlOnBranch' :: (b -> a -> b) -> b -> BinTree a -> BinTree a -> b
      foldlOnBranch' step acc a b =
        case a of
          Leaf c ->
            foldl' step (step acc c) b
          Branch c d ->
            foldlOnBranch' step acc c (Branch d b)

  foldMap :: Monoid m => (a -> m) -> BinTree a -> m
  foldMap =
    foldMapTo mempty
    where
      foldMapTo :: Monoid m => m -> (a -> m) -> BinTree a -> m
      foldMapTo acc map =
        \ case
          Branch a b -> foldMapToOnBranch acc map a b
          Leaf a -> acc <> map a
      foldMapToOnBranch :: Monoid m => m -> (a -> m) -> BinTree a -> BinTree a -> m
      foldMapToOnBranch acc map a b =
        case a of
          Leaf c -> foldMapTo (acc <> map c) map b
          Branch c d -> foldMapToOnBranch acc map c (Branch d b)

#if MIN_VERSION_base(4,13,0)
  foldMap' :: Monoid m => (a -> m) -> BinTree a -> m
  foldMap' =
    foldMapTo' mempty
    where
      foldMapTo' :: Monoid m => m -> (a -> m) -> BinTree a -> m
      foldMapTo' !acc map =
        \ case
          Branch a b -> foldMapToOnBranch' acc map a b
          Leaf a -> acc <> map a
      foldMapToOnBranch' :: Monoid m => m -> (a -> m) -> BinTree a -> BinTree a -> m
      foldMapToOnBranch' acc map a b =
        case a of
          Leaf c -> foldMapTo' (acc <> map c) map b
          Branch c d -> foldMapToOnBranch' acc map c (Branch d b)
#endif

instance Traversable BinTree where

  traverse :: Applicative f => (a -> f b) -> BinTree a -> f (BinTree b)
  traverse map =
    \ case
      Branch a b ->
        traverseOnBranch map a b
      Leaf a ->
        Leaf <$> map a
    where
      traverseOnBranch :: Applicative f => (a -> f b) -> BinTree a -> BinTree a -> f (BinTree b)
      traverseOnBranch map a b =
        case a of
          Leaf c ->
            Branch <$> Leaf <$> map c <*> traverse map b
          Branch c d ->
            traverseOnBranch map a (Branch d b)

instance Foldable1 BinTree where

  fold1 :: Semigroup m => BinTree m -> m
  fold1 =
    \ case
      Branch l r ->
        rebalancingLeft l r (foldl' (<>))
      Leaf a ->
        a

  foldMap1 :: Semigroup m => (a -> m) -> BinTree a -> m
  foldMap1 f =
    \ case
      Branch l r ->
        rebalancingLeft l r (foldl' (\ m a -> m <> f a) . f)
      Leaf a ->
        f a

  toNonEmpty :: BinTree a -> NonEmpty a
  toNonEmpty =
    findFirst
    where
      findFirst =
        \ case
          Branch l r ->
            findFirstOnBranch l r
          Leaf a ->
            a :| []
      findFirstOnBranch l r =
        case l of
          Branch ll lr ->
            findFirstOnBranch ll (Branch lr r)
          Leaf a ->
            a :| foldr (:) [] r

instance Traversable1 BinTree where

  traverse1 map =
    \ case
      Branch a b ->
        traverseOnBranch map a b
      Leaf a ->
        Leaf <$> map a
    where
      traverseOnBranch map a b =
        case a of
          Leaf c ->
            Branch <$> Leaf <$> map c <.> traverse1 map b
          Branch c d ->
            traverseOnBranch map a (Branch d b)

instance Alt BinTree where
  (<!>) =
    Branch

rebalancingLeft :: BinTree a -> BinTree a -> (a -> BinTree a -> b) -> b
rebalancingLeft l r cont =
  case l of
    Branch ll lr ->
      rebalancingLeft ll (Branch lr r) cont
    Leaf a ->
      cont a r

foldM :: Monad m => (a -> b -> m a) -> a -> BinTree b -> m a
foldM step !acc =
  \ case
    Branch a b -> foldMOnBranch step acc a b
    Leaf a -> step acc a
  where
    foldMOnBranch :: Monad m => (a -> b -> m a) -> a -> BinTree b -> BinTree b -> m a
    foldMOnBranch step acc a b =
      case a of
        Leaf c -> step acc c >>= \ acc' -> foldM step acc' b
        Branch c d -> foldMOnBranch step acc c (Branch d b)

fromList1 :: a -> [a] -> BinTree a
fromList1 a =
  \ case
    b : c -> fromList1To (Leaf a) b c
    _ -> Leaf a

fromList1To :: BinTree a -> a -> [a] -> BinTree a
fromList1To leftTree a =
  \ case
    b : c -> fromList1To (Branch leftTree (Leaf a)) b c
    _ -> Branch leftTree (Leaf a)

uncons :: BinTree a -> (a, Maybe (BinTree a))
uncons =
  \ case
    Branch l r ->
      fmap Just (unconsTo r l)
    Leaf a ->
      (a, Nothing)

unconsTo :: BinTree a -> BinTree a -> (a, BinTree a)
unconsTo buff =
  \ case
    Branch l r ->
      unconsTo (Branch r buff) l
    Leaf a ->
      (a, buff)

unsnoc :: BinTree a -> (a, Maybe (BinTree a))
unsnoc =
  \ case
    Branch l r ->
      fmap Just (unsnocTo l r)
    Leaf a ->
      (a, Nothing)

unsnocTo :: BinTree a -> BinTree a -> (a, BinTree a)
unsnocTo buff =
  \ case
    Branch l r ->
      unsnocTo (Branch l buff) r
    Leaf a ->
      (a, buff)

appendEnumFromTo :: (Enum a, Ord a) => a -> a -> BinTree a -> BinTree a
appendEnumFromTo from to =
  if from <= to
    then
      appendEnumFromTo (succ from) to . flip Branch (Leaf from)
    else
      id
