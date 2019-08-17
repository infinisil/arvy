module Evaluation.Weights where

import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed
import           Evaluation.Types

-- | @weightDistribution weights delta@ calculates the distribution of @weights@ by counting how many are in each bin, where bins are @delta@ wide, starting from 0.
-- So with 'delta = 0.2' and a result of '[ 1, 2, 1]' this means that there is one weight between 0 and 0.2 (exclusive), two weights between 0.2 and 0.4, and another one between 0.4 and 0.6
weightDistribution :: Env -> Double -> UArray Int Int
weightDistribution Env { envWeights = weights } delta = runSTUArray $ do
  arr <- newArray (0, maxBin) 0
  forM_ (indices weights) $ \i -> when (uncurry (/=) i) $ do
    let bin = floor (weights ! i / delta)
    old <- readArray arr bin
    writeArray arr bin (old + 1)
  return arr
  where
    maxWeight = maximum (elems weights)
    maxBin = floor (maxWeight / delta)
