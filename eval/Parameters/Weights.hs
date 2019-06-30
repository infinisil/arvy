{-# LANGUAGE BlockArguments #-}

module Parameters.Weights
  ( WeightsParameter(..)
  , ring
  , barabasiAlbert
  , floydWarshall
  , shortestPathWeights
  ) where

import Arvy.Local
import Utils

import Polysemy
import Polysemy.RandomFu
import qualified Algebra.Graph.Class as G
import qualified Algebra.Graph.AdjacencyIntMap as GA
import Control.Monad
import Data.List (foldl')
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as IntMultiSet
import Data.Random.Distribution.Uniform
import Data.Array.MArray
import Data.Array.ST


data WeightsParameter r = WeightsParameter
  { weightsName :: String
  , weightsGet  :: Int -> Sem r GraphWeights
  }


-- TODO: Does this really not use any additional storage?
-- | Generate weights for all vertex pairs from an underlying incomplete graph by calculating the shortest path between them. The Floyd-Warshall algorithm is used to compute this, so complexity is /O(m + n^3)/ with n being the number of vertices and m being the number of edges, no additional space except the resulting weights itself is used. Edge weights in the underlying graph are always assumed to be 1. Use 'symmetricClosure' on the argument to force an undirected graph.
shortestPathWeights :: NodeCount -> GA.AdjacencyIntMap -> GraphWeights
shortestPathWeights n graph = runSTUArray $ do
  -- Initialize array with all edges being infinity, representing no paths between any nodes
  weights <- newArray ((0, 0), (n - 1, n - 1)) infinity
  
  -- Set all known edges to weight 1
  forM_ (GA.edgeList graph) $ \edge ->
    writeArray weights edge 1

  -- Set all weights from nodes to themselves to 0
  forM_ (GA.vertexList graph) $ \x ->
    -- Catch invalid indices early. Could also implement automatix shifting down of
    -- indices to fill all holes, but this shouldn't happen anyways for normal usage
    if x >= n
    then error $ "shortestPathWeights: Graph has vertex with index " ++ show x
      ++ " which is above or equal the number of total vertices " ++ show n
      ++ ". Some vertex index under that is not present in the graph"
      ++ ", shift all node indices down to fill the range."
    else writeArray weights (x, x) 0


  -- Compute shortest paths
  floydWarshall n weights

  return weights

ring :: WeightsParameter r
ring = WeightsParameter
  { weightsName = "Ring graph"
  , weightsGet = \n ->
      return $ shortestPathWeights n $ GA.symmetricClosure $ GA.circuit [0..n-1]
  }

  
--erdosRenyi :: Member (Lift IO) r => WeightsParameter r
--erdosRenyi = WeightsParameter
--  { weightsName = "Erdos Renyi"
--  , weightsGet = \n -> do
--      -- According to Erdos and Renyi, a graph is very likely to be connected with p > (1 + e) ln n / n
--      let p = log (fromIntegral n) / fromIntegral n
--      erdosRenyi n p
--  }
--
--
---- TODO: Rewrite graph-generators in terms of polysemy's RandomFu
---- | Erdős-Rényi graph generator from graph-generators
--erdosRenyi
--  :: Member (Lift IO) r
--  => NodeCount -- ^ The number of total nodes
--  -> Double -- ^ The probability of an edge being connected
--  -> Sem r GraphWeights
--erdosRenyi n p = do
--  graphInfo <- sendM $ erdosRenyiGraph' n p
--  -- Overlaying with every single vertex to make sure all of them are present
--  let graph = symmetricClosure $ infoToGraph graphInfo
--  if isConnected (toGraph graph)
--    then return $ shortestPathWeights n graph
--    else erdosRenyi n p

barabasiAlbert :: Member RandomFu r => Int -> WeightsParameter r
barabasiAlbert m = WeightsParameter
  { weightsName = "Barabasi Albert random graph"
  , weightsGet = \n -> do
      graph <- barabasiAlbertGen n m
      return $ shortestPathWeights n $ GA.symmetricClosure graph
  }

-- Adapted for algebraic-graphs and polysemy from graph-generators package

barabasiAlbertGen :: forall r g . (Member RandomFu r, G.Graph g, G.Vertex g ~ Node) => NodeCount -> Int -> Sem r g
barabasiAlbertGen n m = do
  -- Implementation concept: Iterate over nodes [m..n] in a state monad,
  --   building up the edge list
    -- (Our state: repeated nodes, current targets, edges)
  let initState = (IntMultiSet.empty, [0..m-1], G.vertices [0 .. m - 1])
  -- Strategy: Fold over the list, using a BarabasiState als fold state
  let folder :: (IntMultiSet, [Int], g) -> Int -> Sem r (IntMultiSet, [Int], g)
      folder st curNode = do
          let (repeatedNodes, targets, graph) = st
          -- Create new edges (for the current node)
          let newEdges = map (\t -> (curNode, t)) targets
          -- Add nodes to the repeated nodes multiset
          let newRepeatedNodes = foldl' (flip IntMultiSet.insert) repeatedNodes targets
          let newRepeatedNodes' = IntMultiSet.insertMany curNode m newRepeatedNodes
          -- Select the new target set randomly from the repeated nodes
          let repeatedNodesWithSize = (newRepeatedNodes, IntMultiSet.size newRepeatedNodes)
          newTargets <- selectNDistinctRandomElements m repeatedNodesWithSize
          return (newRepeatedNodes', newTargets, graph `G.overlay` G.edges newEdges)
  -- From the final state, we only require the edge list
  (_, _, allEdges) <- foldM folder initState [m..n-1]
  return allEdges

-- | Select the nth element from a multiset occur list, treating it as virtual large list
--   This is significantly faster than building up the entire list and selecting the nth
--   element
selectNth :: Int -> [(Int, Int)] -> Int
selectNth n [] = error $ "Can't select nth element - n is greater than list size (n=" ++ show n ++ ", list empty)"
selectNth n ((a,c):xs)
    | n <= c = a
    | otherwise = selectNth (n-c) xs

-- | Select a single random element from the multiset, with precalculated size
--   Note that the given size must be the total multiset size, not the number of
--   distinct elements in said se
selectRandomElement :: Member RandomFu r => (IntMultiSet, Int) -> Sem r Int
selectRandomElement (ms, msSize) = do
    let msOccurList = IntMultiSet.toOccurList ms
    r <- sampleRVar (integralUniform 0 (msSize - 1))
    return $ selectNth r msOccurList

-- | Select n distinct random elements from a multiset, with
--   This function will fail to terminate if there are less than n distinct
--   elements in the multiset. This function accepts a multiset with
--   precomputed size for performance reasons
selectNDistinctRandomElements :: Member RandomFu r => Int -> (IntMultiSet, Int) -> Sem r [Int]
selectNDistinctRandomElements n t@(ms, msSize)
    | n == msSize = return . map fst . IntMultiSet.toOccurList $ ms
    | msSize < n = error "Can't select n elements from a set with less than n elements"
    | otherwise = IntSet.toList <$> selectNDistinctRandomElementsWorker n t IntSet.empty

-- | Internal recursive worker for selectNDistinctRandomElements
--   Precondition: n > num distinct elems in multiset (not checked).
--   Does not terminate if the precondition doesn't apply.
--   This implementation is quite naive and selects elements randomly until
--   the predefined number of elements are set.
selectNDistinctRandomElementsWorker :: Member RandomFu r => Int -> (IntMultiSet, Int) -> IntSet -> Sem r IntSet
selectNDistinctRandomElementsWorker 0 _ current = return current
selectNDistinctRandomElementsWorker n t current = do
  randomElement <- selectRandomElement t
  let currentWithRE = IntSet.insert randomElement current
  if randomElement `IntSet.member` current
      then selectNDistinctRandomElementsWorker n t current
      else selectNDistinctRandomElementsWorker (n-1) t currentWithRE
