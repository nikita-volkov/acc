{-# LANGUAGE CPP #-}
module Acc.BinTree1
(
  BinTree1(..),
  foldM,
  fromList1,
  uncons,
  unconsTo,
  unsnoc,
  unsnocTo,
  toNonEmpty,
)
where

import Acc.Prelude hiding (foldM)
import qualified Acc.Prelude as Prelude


data BinTree1 a =
  Leaf !a |
  Branch !(BinTree1 a) !(BinTree1 a)
  deriving (Generic, Generic1, Show)

instance NFData a => NFData (BinTree1 a)

instance NFData1 BinTree1

deriving instance Functor BinTree1

instance Applicative BinTree1 where
  pure =
    Leaf
  (<*>) =
    \ case
      Branch a b ->
        \ c ->
          Branch (a <*> c) (b <*> c)
      Leaf a ->
        fmap a 

instance Foldable BinTree1 where
  
  foldr :: (a -> b -> b) -> b -> BinTree1 a -> b
  foldr step acc =
    \ case
      Branch a b ->
        foldrOnBranch step acc a b
      Leaf a ->
        step a acc
    where
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
    where
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
    where
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
    where
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
    where
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

#if MIN_VERSION_base(4,13,0)
  foldMap' :: Monoid m => (a -> m) -> BinTree1 a -> m
  foldMap' =
    foldMapTo' mempty
    where
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
#endif

instance Traversable BinTree1 where

  traverse :: Applicative f => (a -> f b) -> BinTree1 a -> f (BinTree1 b)
  traverse map =
    \ case
      Branch a b ->
        traverseOnBranch map a b
      Leaf a ->
        Leaf <$> map a
    where
      traverseOnBranch :: Applicative f => (a -> f b) -> BinTree1 a -> BinTree1 a -> f (BinTree1 b)
      traverseOnBranch map a b =
        case a of
          Leaf c ->
            Branch <$> Leaf <$> map c <*> traverse map b
          Branch c d ->
            traverseOnBranch map a (Branch d b)


foldM :: Monad m => (a -> b -> m a) -> a -> BinTree1 b -> m a
foldM step !acc =
  \ case
    Branch a b -> foldMOnBranch step acc a b
    Leaf a -> step acc a
  where
    foldMOnBranch :: Monad m => (a -> b -> m a) -> a -> BinTree1 b -> BinTree1 b -> m a
    foldMOnBranch step acc a b =
      case a of
        Leaf c -> step acc c >>= \ acc' -> foldM step acc' b
        Branch c d -> foldMOnBranch step acc c (Branch d b)

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

toNonEmpty :: BinTree1 a -> NonEmpty a
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
