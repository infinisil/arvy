{-# LANGUAGE TemplateHaskell     #-}
module Arvy.Weights where

import Algebra.Graph.AdjacencyIntMap hiding (edge)
import Arvy.Utils
import Control.Monad
import Data.Array.ST
import Data.Array.Base
import Polysemy
import Data.Graph.Generators.Random.BarabasiAlbert
import Data.Graph.Generators.Random.ErdosRenyi
import Algebra.Graph.ToGraph (toGraph)


-- | The type of our nodes (indices)
type Node = Int
-- | The type of a count of nodes
type NodeCount = Node
-- | The type of our edges
type Edge = (Node, Node)
-- | The type to use for edge weights
type Weight = Double

-- | The type to represent graph weights in a complete graph with a certain array type @arr@
type GraphWeightsArr arr = arr Edge Weight
-- | The type to represent immutable graph weights
type GraphWeights = GraphWeightsArr UArray

-- TODO: Use abstract i type
-- | An effect for providing access to weights from a current node to others
data LocalWeights (m :: * -> *) a where
  WeightTo :: Node -> LocalWeights m Weight

makeSem ''LocalWeights

-- | Run local weights with weights in a matrix and a current node
runLocalWeights :: GraphWeights -> Node -> Sem (LocalWeights ': r) a -> Sem r a
runLocalWeights weights src = interpret $ \case
  WeightTo dst -> return $ weights ! (src, dst)

{-# INLINE floydWarshall #-}
-- TODO: Split a lot of these things out of this Arvy module into the arvy-eval component
-- | Does the main operation in the floyd-warshall algorithm. Computes the shortest path between all nodes by iteratively modifying given weights. Complexity /O(n^3)/
floydWarshall :: MArray arr Weight m => NodeCount -> GraphWeightsArr arr -> m ()
floydWarshall n weights =
  forM_ [0..n - 1] $ \k ->
    forM_ [0..n - 1] $ \i ->
      forM_ [0..n - 1] $ \j -> do
        ij <- unsafeRead weights (i * n + j)
        ik <- unsafeRead weights (i * n + k)
        kj <- unsafeRead weights (k * n + j)
        let ikj = ik + kj
        when (ij > ikj) $
          unsafeWrite weights (i * n + j) ikj

-- TODO: Does this really not use any additional storage?
-- | Generate weights for all vertex pairs from an underlying incomplete graph by calculating the shortest path between them. The Floyd-Warshall algorithm is used to compute this, so complexity is /O(m + n^3)/ with n being the number of vertices and m being the number of edges, no additional space except the resulting weights itself is used. Edge weights in the underlying graph are always assumed to be 1. Use 'symmetricClosure' on the argument to force an undirected graph.
shortestPathWeights :: NodeCount -> AdjacencyIntMap -> GraphWeights
shortestPathWeights n graph = runSTUArray $ do
  -- Initialize array with all edges being infinity, representing no paths between any nodes
  weights <- newArray ((0, 0), (n - 1, n - 1)) infinity
  
  -- Set all known edges to weight 1
  forM_ (edgeList graph) $ \edge ->
    writeArray weights edge 1

  -- Set all weights from nodes to themselves to 0
  forM_ (vertexList graph) $ \x ->
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


-- TODO: Rewrite graph-generators in terms of polysemy's RandomFu
-- TODO: Implement in terms of algebraic-graphs, see also https://github.com/snowleopard/alga/issues/196
-- | Barabasi-Albert graph generator from graph-generators
barabasiAlbert
  :: Member (Lift IO) r
  => NodeCount -- ^ The number of total nodes
  -> NodeCount -- ^ The number of previous to connect every new node, must be greater than 1
  -> Sem r GraphWeights
barabasiAlbert n m = do
  graphInfo <- sendM $ barabasiAlbertGraph' n m
  return $ shortestPathWeights n $ symmetricClosure $ infoToGraph graphInfo
  
-- TODO: Rewrite graph-generators in terms of polysemy's RandomFu
-- | Erdős-Rényi graph generator from graph-generators
erdosRenyi
  :: Member (Lift IO) r
  => NodeCount -- ^ The number of total nodes
  -> Double -- ^ The probability of an edge being connected
  -> Sem r GraphWeights
erdosRenyi n p = do
  graphInfo <- sendM $ erdosRenyiGraph' n p
  -- Overlaying with every single vertex to make sure all of them are present
  let graph = symmetricClosure $ infoToGraph graphInfo
  if isConnected (toGraph graph)
    then return $ shortestPathWeights n graph
    else erdosRenyi n p

-- | Ring weights
ringWeights :: NodeCount -> GraphWeights
ringWeights n = shortestPathWeights n $ symmetricClosure $ circuit [0..n-1]
