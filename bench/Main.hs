module Main where

import qualified Acc
import qualified Data.DList as DList
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Sequence
import qualified Data.Vector as Vector
import Gauge
import Gauge.Main
import Prelude

main =
  defaultMain
    [ bgroup "sum" $
        [ byMagnitudeUpTo "cons" 3 $ \size ->
            let !input = force $ enumFromTo 0 size :: [Int]
             in [ sumBench "acc" input sum $
                    foldl' (flip Acc.cons) mempty,
                  sumBench "list" input sum $
                    foldl' (flip (:)) [],
                  sumBench "dlist" input sum $
                    foldl' (flip DList.cons) mempty,
                  sumBench "sequence" input sum $
                    foldl' (flip (Sequence.<|)) mempty
                ]
        ]
    ]

-- |
-- Construct a benchmark that measures construction of the intermediate representation
-- and folding using sum.
--
-- As a bonus,
-- before benchmarking it ensures that the conversion is correct
-- by using the input as the reference.
{-# NOINLINE sumBench #-}
sumBench ::
  -- | Benchmark name.
  String ->
  -- | Input sample.
  [Int] ->
  -- | Reducer of the intermediate representation to sum.
  (acc -> Int) ->
  -- | Constructor of the intermediate representation.
  ([Int] -> acc) ->
  Benchmark
sumBench name list reducer constructor =
  if sum list == reducer (constructor list)
    then bench name $ nf (reducer . constructor) list
    else error $ "Conversion is incorrect on " <> name

byMagnitudeUpTo :: String -> Int -> (Int -> [Benchmark]) -> Benchmark
byMagnitudeUpTo groupName amount benchmarks =
  bgroup groupName $
    take amount sizesByMagnitude <&> \size -> bgroup (show size) (benchmarks size)

sizesByMagnitude :: [Int]
sizesByMagnitude = [0 ..] <&> \magnitude -> 10 ^ (2 * magnitude)
