{-# LANGUAGE CPP #-}
module Acc.NeAcc.Def
(
  NeAcc(..),
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


{-|
Non-empty accumulator.

Relates to 'Acc.Acc' the same way as 'NonEmpty' to list.
-}
data NeAcc a =
  Leaf !a |
  Branch !(NeAcc a) !(NeAcc a)
  deriving (Generic, Generic1)

instance Show a => Show (NeAcc a) where
  show =
    show . toList

instance NFData a => NFData (NeAcc a)

instance NFData1 NeAcc

instance IsList (NeAcc a) where
  type Item (NeAcc a) = a
  {-# INLINE fromList #-}
  fromList =
    \ case
      a : b -> fromList1 a b
      _ -> error "Empty input list"
  {-# INLINE toList #-}
  toList =
    foldr (:) []

deriving instance Functor NeAcc

instance Applicative NeAcc where
  pure =
    Leaf
  {-# INLINE (<*>) #-}
  (<*>) =
    \ case
      Branch a b ->
        \ c ->
          Branch (a <*> c) (b <*> c)
      Leaf a ->
        fmap a 

instance Foldable NeAcc where
  
  {-# INLINABLE foldr #-}
  foldr :: (a -> b -> b) -> b -> NeAcc a -> b
  foldr step acc =
    peel []
    where
      peel layers =
        \ case
          Leaf a ->
            step a (unpeel layers)
          Branch l r ->
            peel (r : layers) l
      unpeel =
        \ case
          h : t ->
            peel t h
          _ ->
            acc

  {-# INLINE foldr' #-}
  foldr' :: (a -> b -> b) -> b -> NeAcc a -> b
  foldr' step =
    peel []
    where
      peel layers acc =
        \ case
          Leaf a ->
            unpeel (step a acc) layers
          Branch l r ->
            peel (l : layers) acc r
      unpeel !acc =
        \ case
          h : t ->
            peel t acc h
          _ ->
            acc
  
  {-# INLINE foldl #-}
  foldl :: (b -> a -> b) -> b -> NeAcc a -> b
  foldl step acc =
    \ case
      Branch a b ->
        foldlOnBranch step acc a b
      Leaf a ->
        step acc a
    where
      foldlOnBranch :: (b -> a -> b) -> b -> NeAcc a -> NeAcc a -> b
      foldlOnBranch step acc a b =
        case b of
          Leaf c ->
            step (foldl step acc a) c
          Branch c d ->
            foldlOnBranch step acc (Branch a c) d

  {-# INLINE foldl' #-}
  foldl' :: (b -> a -> b) -> b -> NeAcc a -> b
  foldl' step !acc =
    \ case
      Branch a b ->
        foldlOnBranch' step acc a b
      Leaf a ->
        step acc a
    where
      foldlOnBranch' :: (b -> a -> b) -> b -> NeAcc a -> NeAcc a -> b
      foldlOnBranch' step acc a b =
        case a of
          Leaf c ->
            foldl' step (step acc c) b
          Branch c d ->
            foldlOnBranch' step acc c (Branch d b)

  {-# INLINE foldMap #-}
  foldMap :: Monoid m => (a -> m) -> NeAcc a -> m
  foldMap map =
    peel
    where
      peel =
        \ case
          Branch a b ->
            peelLeftStacking b a
          Leaf a ->
            map a
      peelLeftStacking buff =
        \ case
          Branch a b ->
            peelLeftStacking (Branch b buff) a
          Leaf a ->
            map a <> peel buff

#if MIN_VERSION_base(4,13,0)
  {-# INLINE foldMap' #-}
  foldMap' :: Monoid m => (a -> m) -> NeAcc a -> m
  foldMap' =
    foldMapTo' mempty
    where
      foldMapTo' :: Monoid m => m -> (a -> m) -> NeAcc a -> m
      foldMapTo' !acc map =
        \ case
          Branch a b -> foldMapToOnBranch' acc map a b
          Leaf a -> acc <> map a
      foldMapToOnBranch' :: Monoid m => m -> (a -> m) -> NeAcc a -> NeAcc a -> m
      foldMapToOnBranch' acc map a b =
        case a of
          Leaf c -> foldMapTo' (acc <> map c) map b
          Branch c d -> foldMapToOnBranch' acc map c (Branch d b)
#endif

instance Traversable NeAcc where

  traverse :: Applicative f => (a -> f b) -> NeAcc a -> f (NeAcc b)
  traverse map =
    \ case
      Branch a b ->
        traverseOnBranch map a b
      Leaf a ->
        Leaf <$> map a
    where
      traverseOnBranch :: Applicative f => (a -> f b) -> NeAcc a -> NeAcc a -> f (NeAcc b)
      traverseOnBranch map a b =
        case a of
          Leaf c ->
            Branch <$> Leaf <$> map c <*> traverse map b
          Branch c d ->
            traverseOnBranch map a (Branch d b)

instance Foldable1 NeAcc where

  fold1 :: Semigroup m => NeAcc m -> m
  fold1 =
    \ case
      Branch l r ->
        rebalancingLeft l r (foldl' (<>))
      Leaf a ->
        a

  foldMap1 :: Semigroup m => (a -> m) -> NeAcc a -> m
  foldMap1 f =
    \ case
      Branch l r ->
        rebalancingLeft l r (foldl' (\ m a -> m <> f a) . f)
      Leaf a ->
        f a

  toNonEmpty :: NeAcc a -> NonEmpty a
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

instance Traversable1 NeAcc where

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

instance Alt NeAcc where
  {-# INLINE [1] (<!>) #-}
  (<!>) =
    Branch

instance Semigroup (NeAcc a) where
  {-# INLINE [1] (<>) #-}
  (<>) =
    Branch

{-# INLINE rebalancingLeft #-}
rebalancingLeft :: NeAcc a -> NeAcc a -> (a -> NeAcc a -> b) -> b
rebalancingLeft l r cont =
  case l of
    Branch ll lr ->
      rebalancingLeft ll (Branch lr r) cont
    Leaf a ->
      cont a r

foldM :: Monad m => (a -> b -> m a) -> a -> NeAcc b -> m a
foldM step !acc =
  \ case
    Branch a b -> foldMOnBranch step acc a b
    Leaf a -> step acc a
  where
    foldMOnBranch :: Monad m => (a -> b -> m a) -> a -> NeAcc b -> NeAcc b -> m a
    foldMOnBranch step acc a b =
      case a of
        Leaf c -> step acc c >>= \ acc' -> foldM step acc' b
        Branch c d -> foldMOnBranch step acc c (Branch d b)

fromList1 :: a -> [a] -> NeAcc a
fromList1 a =
  \ case
    b : c -> fromList1To (Leaf a) b c
    _ -> Leaf a

fromList1To :: NeAcc a -> a -> [a] -> NeAcc a
fromList1To leftTree a =
  \ case
    b : c -> fromList1To (Branch leftTree (Leaf a)) b c
    _ -> Branch leftTree (Leaf a)

{-# INLINE uncons #-}
uncons :: NeAcc a -> (a, Maybe (NeAcc a))
uncons =
  \ case
    Branch l r ->
      fmap Just (unconsTo r l)
    Leaf a ->
      (a, Nothing)

{-# INLINE unconsTo #-}
unconsTo :: NeAcc a -> NeAcc a -> (a, NeAcc a)
unconsTo buff =
  \ case
    Branch l r ->
      unconsTo (Branch r buff) l
    Leaf a ->
      (a, buff)

unsnoc :: NeAcc a -> (a, Maybe (NeAcc a))
unsnoc =
  \ case
    Branch l r ->
      fmap Just (unsnocTo l r)
    Leaf a ->
      (a, Nothing)

unsnocTo :: NeAcc a -> NeAcc a -> (a, NeAcc a)
unsnocTo buff =
  \ case
    Branch l r ->
      unsnocTo (Branch l buff) r
    Leaf a ->
      (a, buff)

appendEnumFromTo :: (Enum a, Ord a) => a -> a -> NeAcc a -> NeAcc a
appendEnumFromTo from to =
  if from <= to
    then
      appendEnumFromTo (succ from) to . flip Branch (Leaf from)
    else
      id
