module Main where

import Prelude
import Criterion
import Criterion.Main
import qualified Acc
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.DList as DList
import qualified Data.Vector as Vector


main =
  defaultMain [
    sumBgroup "1"
      (replicate 1000 1)
      (foldMapToRight)
    ,
    sumBgroup "2"
      (replicate 1000 1)
      (foldMapToLeft)
    ,
    sumBgroup "sum/foldr',foldr'"
      (Vector.fromList (replicate 100 (Vector.fromList (replicate @Int 100 1))))
      (\ singleton -> foldr' (\ a b -> foldr' (mappend . singleton) mempty a <> b) mempty)
    ,
    sumBgroup "sum/foldl',foldl'"
      (Vector.fromList (replicate 100 (Vector.fromList (replicate @Int 100 1))))
      (\ singleton -> foldl' (\ a b -> a <> foldl' (\ a -> mappend a . singleton) mempty b) mempty)
    ,
    bgroup "thousand-elements" [
      bgroup "foldl'" $ let
        !input =
          force $ enumFromTo 0 999 :: [Int]
        in [
          bench "Acc" $ let
            work input =
              let
                acc =
                  foldl' (<>) mempty $ fmap (pure @Acc.Acc) input
                in Foldable.toList acc
            in nf work input
          ,
          bench "Seq" $ let
            work input =
              let
                seq =
                  foldl' (<>) mempty $ fmap Seq.singleton input
                in Foldable.toList seq
            in nf work input
          ,
          bench "DList" $ let
            work input =
              let
                seq =
                  foldl' (<>) mempty $ fmap DList.singleton input
                in Foldable.toList seq
            in nf work input
          ]
      ,
      bgroup "foldr" $ let
        !input =
          force $ enumFromTo 0 999 :: [Int]
        in [
          bench "Acc" $ let
            work input =
              let
                acc =
                  foldr (<>) mempty $ fmap (pure @Acc.Acc) input
                in Foldable.toList acc
            in nf work input
          ,
          bench "Seq" $ let
            work input =
              let
                seq =
                  foldr (<>) mempty $ fmap Seq.singleton input
                in Foldable.toList seq
            in nf work input
          ,
          bench "DList" $ let
            work input =
              let
                seq =
                  foldr (<>) mempty $ fmap DList.singleton input
                in Foldable.toList seq
            in nf work input
          ]
      ,
      bgroup "foldr'" $ let
        !input =
          force $ enumFromTo 0 999 :: [Int]
        in [
          bench "Acc" $ let
            work input =
              let
                acc =
                  foldr' (<>) mempty $ fmap (pure @Acc.Acc) input
                in Foldable.toList acc
            in nf work input
          ,
          bench "Seq" $ let
            work input =
              let
                seq =
                  foldr' (<>) mempty $ fmap Seq.singleton input
                in Foldable.toList seq
            in nf work input
          ,
          bench "DList" $ let
            work input =
              let
                seq =
                  foldr' (<>) mempty $ fmap DList.singleton input
                in Foldable.toList seq
            in nf work input
          ]
      ,
      bgroup "foldr, force intermediate" $ let
        !input =
          force $ enumFromTo 0 999 :: [Int]
        in [
          bench "Acc" $ let
            work input =
              let
                acc =
                  foldr (<>) mempty $ fmap (pure @Acc.Acc) input
                in Foldable.toList $!! acc
            in nf work input
          ,
          bench "Seq" $ let
            work input =
              let
                seq =
                  foldr (<>) mempty $ fmap Seq.singleton input
                in Foldable.toList $!! seq
            in nf work input
          ,
          bench "DList" $ let
            work input =
              let
                seq =
                  foldr (<>) mempty $ fmap DList.singleton input
                in Foldable.toList $!! seq
            in nf work input
          ]
      ,
      bgroup "foldMap" $ let
        !input =
          force $ enumFromTo 0 999 :: [Int]
        in [
          bench "Acc" $ let
            work input =
              let
                acc =
                  foldMap (pure @Acc.Acc) input
                in Foldable.toList acc
            in nf work input
          ,
          bench "Seq" $ let
            work input =
              let
                seq =
                  foldMap Seq.singleton input
                in Foldable.toList seq
            in nf work input
          ,
          bench "DList" $ let
            work input =
              let
                seq =
                  foldMap DList.singleton input
                in Foldable.toList seq
            in nf work input
          ]
      ,
      bgroup "foldMap'" $ let
        !input =
          force $ enumFromTo 0 999 :: [Int]
        in [
          bench "Acc" $ let
            work input =
              let
                acc =
                  foldMap' (pure @Acc.Acc) input
                in Foldable.toList acc
            in nf work input
          ,
          bench "Seq" $ let
            work input =
              let
                seq =
                  foldMap' Seq.singleton input
                in Foldable.toList seq
            in nf work input
          ,
          bench "DList" $ let
            work input =
              let
                seq =
                  foldMap' DList.singleton input
                in Foldable.toList seq
            in nf work input
          ]
      ]
    ,
    bgroup "groups" [
      bgroup "foldl'" [
        bench "acc" $ let
          input :: [Acc.Acc Int]
          !input =
            enumFromTo 0 99 & fmap pure &
            foldl' (<>) mempty &
            replicate 10 &
            force
          work =
            Foldable.toList . foldl' (<>) mempty
          in nf work input
        ,
        bench "seq" $ let
          input :: [Seq Int]
          !input =
            enumFromTo 0 99 & fmap pure &
            foldl' (<>) mempty &
            replicate 10 &
            force
          work =
            Foldable.toList . foldl' (<>) mempty
          in nf work input
        ,
        bench "dlist" $ let
          input :: [DList Int]
          !input =
            enumFromTo 0 99 & fmap pure &
            foldl' (<>) mempty &
            replicate 10 &
            force
          work =
            Foldable.toList . foldl' (<>) mempty
          in nf work input
        ]
      ]
    ]

sumBgroup :: NFData input => String -> input -> (forall f. (Foldable f, Monoid (f Int)) => (Int -> f Int) -> input -> f Int) -> Benchmark
sumBgroup name (force -> !input) build =
  bgroup name [
    sumBench "Acc" (pure @Acc.Acc)
    ,
    sumBench "Seq" Seq.singleton
    ,
    sumBench "DList" DList.singleton
    ,
    sumBench "List" (pure @[])
    ]
  where
    sumBench :: (Foldable f, Monoid (f Int)) => String -> (Int -> f Int) -> Benchmark
    sumBench name singleton =
      bench name (nf (Foldable.sum . build singleton) input)



{-| Best for Acc. -}
{-# NOINLINE foldMapToRight #-}
foldMapToRight :: Monoid m => (a -> m) -> [a] -> m
foldMapToRight pure =
  foldl' (\ a b -> pure b <> a) mempty

{-| Worst for Acc. -}
{-# NOINLINE foldMapToLeft #-}
foldMapToLeft :: Monoid m => (a -> m) -> [a] -> m
foldMapToLeft pure =
  foldl' (\ a b -> a <> pure b) mempty
