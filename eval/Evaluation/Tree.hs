module Evaluation.Tree where

import Arvy.Algorithm

import           Data.Array.IO
import           Data.Array.Unboxed
import           Data.IntMap        (IntMap)
import qualified Data.IntMap        as IntMap
import           Data.IntSet        (IntSet)
import qualified Data.IntSet        as IntSet
import Prelude
import Evaluation.Types
import Utils
import Control.Monad
import Control.DeepSeq

totalTreeWeight :: Env -> IO Double
totalTreeWeight Env { envWeights = weights, envTree = tree } = sumMapAssocs (weights !) tree

-- | Shortest paths between all nodes in the current tree
shortestTreePaths :: Env -> IO GraphWeights
shortestTreePaths Env { envNodeCount = n, envWeights = weights, envTree = tree } = do
  shortest :: IOUArray Edge Double <- newArray ((0, 0), (n - 1, n - 1)) infinity
  bnds <- getBounds tree

  forM_ (range bnds) $ \i -> do
    writeArray shortest (i, i) 0
    e <- readArray tree i
    when (i /= e) $ do
      writeArray shortest (i, e) (weights ! (i, e))
      writeArray shortest (e, i) (weights ! (i, e))

  floydWarshall n shortest
  freeze shortest

-- | The total weight of all shortest paths between all node pairs
totalPairWeight :: Env -> IO Double
totalPairWeight env = do
  tw <- shortestTreePaths env
  return $! sum (elems tw) / 2

-- TODO: This function makes productivity really bad because it probably generates a whole lot of data
-- | Calculates the average tree stretch given the complete graph weights and a tree. The stretch for a pair of nodes (u, v) is the ratio of the shortest path in the tree over the shortest path in the complete graph (which is assumed to be euclidian, so the shortest path is always directly the edge (u, v)). The average tree stretch is the average stretch over all node pairs (u, v) with u != v. Complexity /O(n^2)/
avgTreeStretchDiameter :: Env -> IO (Double, Double)
avgTreeStretchDiameter Env { envNodeCount = n, envWeights = weights, envTree = tree' } = do
  -- TODO: Trace adjacency map with successor changes, which should be faster than rebuilding it all the time
  graph <- IntMap.unionsWith IntSet.union . map bidirEdge <$> getAssocs tree'
  return $!! go graph
  where
    bidirEdge :: (Node, Node) -> IntMap IntSet
    bidirEdge (a, b)
      | a == b = IntMap.empty
      | otherwise = IntMap.singleton a (IntSet.singleton b) `IntMap.union` IntMap.singleton b (IntSet.singleton a)

    go :: IntMap IntSet -> (Double, Double)
    go graph = (sum summed / fromIntegral (n * (n - 1)), maximum maxed) where
      (summed, maxed) = unzip [ summedTreeStretch root | root <- [0 .. n - 1] ]


      adjacent :: Node -> IntSet
      adjacent node = IntMap.findWithDefault (error $ "No node " ++ show node ++ ", inconsistent rooted tree") node graph

      -- | Sums up all tree stretches from the given 'Node' to all others. Complexity /O(n)/
      summedTreeStretch :: Node -> (Double, Double)
      summedTreeStretch root = go' root (adjacent root) 0 where
        go' :: Node -> IntSet -> Double -> (Double, Double)
        go' parent children baseWeight = result where
          result = IntSet.foldl' onChildren (if root == parent then baseWeight else baseWeight / weights ! (root, parent), baseWeight) children
          onChildren (acc, maxAcc) child = (acc + summed', max maxAcc maxed') where
            (summed', maxed') = go' child children' (baseWeight + weight)
            weight = weights ! (parent, child)
            children' = IntSet.delete parent $ adjacent child
