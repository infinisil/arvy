module Arvy.Weights where

import           Algebra.Graph.AdjacencyIntMap
import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed

type GraphWeights = UArray (Int, Int) Double


-- | Generate weights for all vertex pairs from an underlying incomplete graph by calculating the shortest path between them. The Floyd-Warshall algorithm is used to compute this, so complexity is O(n^3) with n being the number of edges, no additional space except the resulting weights itself is used. Edge weights in the underlying graph are always assumed to be 1. Use 'symmetricClosure' on the argument to force an undirected graph.
shortestPathWeights :: AdjacencyIntMap -> GraphWeights
shortestPathWeights graph = runSTUArray $ do
  let v = vertexCount graph
  arr <- newArray ((0, 0), (v - 1, v - 1)) (read "Infinity")
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

ringWeights :: Int -> GraphWeights
ringWeights n = shortestPathWeights (symmetricClosure (circuit [0..n - 1]))
