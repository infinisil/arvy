module Evaluation.Tree where

import           Arvy.Local
import           Evaluation

import           Data.Array.IO
import           Data.Array.Unboxed
import           Data.IntMap        (IntMap)
import qualified Data.IntMap        as IntMap
import           Data.IntSet        (IntSet)
import qualified Data.IntSet        as IntSet
import           Polysemy
import           Polysemy.Output



treeStretch :: Int -> GraphWeights -> IOArray Int (Maybe Int) -> Eval a (a, Double)
treeStretch n weights tree = Eval
  { initialState = ()
  , tracing = \event -> do
      t <- sendM $ freeze tree
      output (event, avgTreeStretch n weights t)
  , final = return ()
  }

-- | Calculates the average tree stretch given the complete graph weights and a tree. The stretch for a pair of nodes (u, v) is the ratio of the shortest path in the tree over the shortest path in the complete graph (which is assumed to be euclidian, so the shortest path is always directly the edge (u, v)). The average tree stretch is the average stretch over all node pairs (u, v) with u != v. Complexity /O(n^2)/
avgTreeStretch :: NodeCount -> GraphWeights -> RootedTree -> Double
avgTreeStretch n weights tree = sum [ summedTreeStretch root | root <- [0 .. n - 1] ] / fromIntegral (n * (n - 1)) where

  -- | Converts the rooted tree into an adjacency map graph, represented as a @'IntMap' 'IntSet'@, which is a faster version of @Map Int (Set Int)@, meaning a map from every node to a set of nodes it's adjacent to. This function sets up bidirectional edges. Complexity /O(n)/
  graph :: IntMap IntSet
  graph = IntMap.unionsWith IntSet.union
    $ map bidirEdge
    $ assocs tree
    where
      bidirEdge :: (Node, Maybe Node) -> IntMap IntSet
      bidirEdge (_, Nothing) = IntMap.empty
      bidirEdge (a, Just b) = IntMap.singleton a (IntSet.singleton b) `IntMap.union` IntMap.singleton b (IntSet.singleton a)

  adjacent :: Node -> IntSet
  adjacent node = IntMap.findWithDefault (error "No such node, inconsistent rooted tree") node graph

  -- | Sums up all tree stretches from the given 'Node' to all others. Complexity /O(n)/
  summedTreeStretch :: Node -> Double
  summedTreeStretch root = go root (adjacent root) 0 where
    go :: Node -> IntSet -> Double -> Double
    go parent children baseWeight = summed where
      summed = IntSet.foldl' onChildren (if root == parent then baseWeight else baseWeight / weights ! (root, parent)) children
      onChildren acc child = acc + go child superchildren (baseWeight + weight) where
        weight = weights ! (parent, child)
        superchildren = IntSet.delete parent $ adjacent child
