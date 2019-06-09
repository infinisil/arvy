{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Arvy.Weights
  ( module Arvy.Weights
  , (!)
  ) where

import           Algebra.Graph.AdjacencyIntMap
import           Arvy.Utils
import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed
import           Polysemy
import           Polysemy.Random

type GraphWeights = UArray (Int, Int) Double


-- | Generate weights for all vertex pairs from an underlying incomplete graph by calculating the shortest path between them. The Floyd-Warshall algorithm is used to compute this, so complexity is O(n^3) with n being the number of edges, no additional space except the resulting weights itself is used. Edge weights in the underlying graph are always assumed to be 1. Use 'symmetricClosure' on the argument to force an undirected graph.
shortestPathWeights :: AdjacencyIntMap -> GraphWeights
shortestPathWeights graph = runSTUArray $ do
  let v = vertexCount graph
  arr <- newArray ((0, 0), (v - 1, v - 1)) infinity
  forM_ (vertexList graph) $ \x ->
    -- Catch invalid indices early. Could also implement automatix shifting down of
    -- indices to fill all holes, but this shouldn't happen anyways for normal usage
    if x >= v
    then error $ "shortestPathWeights: Graph has vertex with index " ++ show x
      ++ " which is above or equal the number of total vertices " ++ show v
      ++ ". Some vertex index under that is not present in the graph"
      ++ ", shift all node indices down to fill the range."
    else writeArray arr (x, x) 0

  forM_ (edgeList graph) $ \(a, b) ->
    writeArray arr (a, b) 1


  forM_ [0..v - 1] $ \k ->
    forM_ [0..v - 1] $ \i ->
      forM_ [0..v - 1] $ \j -> do
        ij <- readArray arr (i, j)
        ik <- readArray arr (i, k)
        kj <- readArray arr (k, j)
        let ikj = ik + kj
        when (ij > ikj) $
          writeArray arr (i, j) ikj

  return arr

-- | Generates a number of random 2D points with coordinates from 0.0 to 1.0
randomPoints :: Member Random r => Int -> Sem r (Array Int (Double, Double))
randomPoints count = listArray (0, count - 1) <$> go count where
  go :: Member Random r => Int -> Sem r [(Double, Double)]
  go 0 = return []
  go n = do
    pair <- (,) <$> random <*> random
    (pair:) <$> go (n - 1)

-- | Generates weights based on the euclidian distance for a number of given 2D points
euclidianWeights :: Array Int (Double, Double) -> GraphWeights
euclidianWeights points = array ((low, low), (high, high))
  [ ((i, j), if i == j then 0 else sqrt $ (x1 - x2) ^^ (2 :: Int) + (y1 - y2) ^^ (2 :: Int))
  | i <- [low..high]
  , let (x1, y1) = points ! i
  , j <- [low..high]
  , let (x2, y2) = points ! j
  ] where
  (low, high) = bounds points


ringWeights :: Int -> GraphWeights
ringWeights n = shortestPathWeights (symmetricClosure (circuit [0..n - 1]))
