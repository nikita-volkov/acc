{-# LANGUAGE CPP #-}
module Acc.NeAcc
(
  NeAcc,
)
where

import Acc.Prelude
import qualified Acc.BinTree1 as BinTree1


{-|
Non-empty accumulator.

Relates to 'Acc.Acc' the same way as 'NonEmpty' to list.
-}
newtype NeAcc a =
  NeAcc (BinTree1.BinTree1 a)
  deriving (Generic, Generic1)

instance Show a => Show (NeAcc a) where
  show =
    show . toList

instance NFData a => NFData (NeAcc a)

instance NFData1 NeAcc

instance IsList (NeAcc a) where
  type Item (NeAcc a) = a
  fromList =
    \ case
      a : b -> NeAcc (BinTree1.fromList1 a b)
      _ -> error "Acc.NeAcc.fromList empty input list"
  toList =
    foldr (:) []

instance Semigroup (NeAcc a) where
  (<>) =
    (<!>)

deriving instance Functor NeAcc

deriving instance Applicative NeAcc

instance Alt NeAcc where
  (<!>) (NeAcc l) (NeAcc r) =
    NeAcc (l <!> r)

instance Foldable NeAcc where

  foldMap f (NeAcc tree) =
    foldMap f tree

#if MIN_VERSION_base(4,13,0)
  foldMap' f (NeAcc tree) =
    foldMap' f tree
#endif

  foldr step acc (NeAcc tree) =
    foldr step acc tree

  foldr' step acc (NeAcc tree) =
    foldr' step acc tree

  foldl step acc (NeAcc tree) =
    foldl step acc tree

  foldl' step acc (NeAcc tree) =
    foldl' step acc tree

deriving instance Traversable NeAcc

deriving instance Foldable1 NeAcc

instance Traversable1 NeAcc where
  traverse1 f (NeAcc tree) =
    NeAcc <$> traverse1 f tree
