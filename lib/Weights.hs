module Weights where

import           Data.Array.Unboxed

type GraphWeights = UArray (Int, Int) Double

-- TODO: Implement https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm for https://hackage.haskell.org/package/algebraic-graphs, then use that to generate the weights
full :: Int -> GraphWeights
full n = listArray ((0, 0), (n - 1, n - 1)) (repeat 1) // [ ((i, i), 0) | i <- [0..(n - 1)] ]
