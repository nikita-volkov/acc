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
             in [ reduceConstructBench "acc" input sum $
                    foldl' (flip Acc.cons) mempty,
                  reduceConstructBench "list" input sum $
                    foldl' (flip (:)) [],
                  reduceConstructBench "dlist" input sum $
                    foldl' (flip DList.cons) mempty,
                  reduceConstructBench "sequence" input sum $
                    foldl' (flip (Sequence.<|)) mempty
                ],
          byMagnitudeUpTo "snoc" 3 $ \size ->
            let !input = force $ enumFromTo 0 size :: [Int]
             in [ reduceConstructBench "acc" input sum $
                    foldl' (flip Acc.snoc) mempty,
                  reduceConstructBench "list" input sum $
                    foldl' (\list a -> list <> [a]) mempty,
                  reduceConstructBench "dlist" input sum $
                    foldl' DList.snoc mempty,
                  reduceConstructBench "sequence" input sum $
                    foldl' (Sequence.|>) mempty
                ]
        ],
      bgroup "length" $
        [ byMagnitudeUpTo "cons" 3 $ \size ->
            let !input = force $ enumFromTo 0 size :: [Int]
             in [ reduceConstructBench "acc" input length $
                    foldl' (flip Acc.cons) mempty,
                  reduceConstructBench "list" input length $
                    foldl' (flip (:)) [],
                  reduceConstructBench "dlist" input length $
                    foldl' (flip DList.cons) mempty,
                  reduceConstructBench "sequence" input length $
                    foldl' (flip (Sequence.<|)) mempty
                ]
        ]
    ]

-- |
-- Construct a benchmark that measures construction of the intermediate representation
-- and reduction, ensuring that they don't get fused.
{-# NOINLINE reduceConstructBench #-}
reduceConstructBench ::
  NFData reduction =>
  -- | Benchmark name.
  String ->
  -- | Input sample.
  [a] ->
  -- | Reducer of the intermediate representation.
  (intermediate -> reduction) ->
  -- | Constructor of the intermediate representation.
  ([a] -> intermediate) ->
  Benchmark
reduceConstructBench name list reducer constructor =
  bench name $ nf (reducer . constructor) list

byMagnitudeUpTo :: String -> Int -> (Int -> [Benchmark]) -> Benchmark
byMagnitudeUpTo groupName amount benchmarks =
  bgroup groupName $
    take amount sizesByMagnitude <&> \size -> bgroup (show size) (benchmarks size)

sizesByMagnitude :: [Int]
sizesByMagnitude = [0 ..] <&> \magnitude -> 10 ^ (2 * magnitude)
