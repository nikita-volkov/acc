module Acc.Balance where

import Acc.Prelude
import qualified Acc.Prelude as Prelude

-- |
-- Compact encoding of the size of the tree accompanied by its balance direction.
newtype Balance = Balance Int

instance Semigroup Balance where
  {-# INLINE (<>) #-}
  Balance l <> Balance r =
    Balance $
      if l < 0
        then
          if r < 0
            then
              if l < r
                then negate r
                else l
            else
              if negate l < r
                then r
                else l
        else
          if r < 0
            then case negate r of
              r ->
                if l < r
                  then negate l
                  else r
            else
              if l < r
                then negate l
                else r

instance Monoid Balance where
  mempty = Balance 0

isLeft :: Balance -> Bool
isLeft (Balance n) = n <= 0
