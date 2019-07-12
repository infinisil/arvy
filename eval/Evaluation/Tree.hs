module Evaluation.Tree where

import           Arvy.Local
import           Evaluation
import           Evaluation.Utils
import Evaluation.Request

import           Data.Array.IO
import           Data.Array.Unboxed
import           Data.IntMap        (IntMap)
import qualified Data.IntMap        as IntMap
import           Data.IntSet        (IntSet)
import qualified Data.IntSet        as IntSet
import           Polysemy
import           Polysemy.Output
import           Polysemy.Reader
import Prelude hiding ((.))
import Control.Category

sparseTreeStretch :: Int -> Tracer ArvyEvent Double
sparseTreeStretch n = treeStretch . everyNth n . requests (const ())

treeStretch :: Tracer a Double
treeStretch = Tracer () \_ -> do
  Env { .. } <- ask
  frozen <- sendM $ freeze tree
  let stretch = avgTreeStretch nodeCount weights frozen
  output stretch

-- | Calculates the average tree stretch given the complete graph weights and a tree. The stretch for a pair of nodes (u, v) is the ratio of the shortest path in the tree over the shortest path in the complete graph (which is assumed to be euclidian, so the shortest path is always directly the edge (u, v)). The average tree stretch is the average stretch over all node pairs (u, v) with u != v. Complexity /O(n^2)/
avgTreeStretch :: NodeCount -> GraphWeights -> RootedTree -> Double
avgTreeStretch n weights tree = sum [ summedTreeStretch root | root <- [0 .. n - 1] ] / fromIntegral (n * (n - 1)) where

  -- | Converts the rooted tree into an adjacency map graph, represented as a @'IntMap' 'IntSet'@, which is a faster version of @Map Int (Set Int)@, meaning a map from every node to a set of nodes it's adjacent to. This function sets up bidirectional edges. Complexity /O(n)/
  graph :: IntMap IntSet
  graph = IntMap.unionsWith IntSet.union
    $ map bidirEdge
    $ assocs tree
    where
      bidirEdge :: (Node, Node) -> IntMap IntSet
      bidirEdge (a, b)
        | a == b = IntMap.empty
        | otherwise = IntMap.singleton a (IntSet.singleton b) `IntMap.union` IntMap.singleton b (IntSet.singleton a)

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
