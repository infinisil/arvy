module Evaluation.Tree where

import           Arvy.Local
import           Evaluation
import           Evaluation.Utils
import Utils
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
import Control.Monad

sparseTreeStretchDiameter :: NodeCount -> GraphWeights -> Int -> Tracer ArvyEvent (Double, Double)
sparseTreeStretchDiameter nodeCount weights n = treeStretchDiameter nodeCount weights . everyNth n . requests (const ())

treeStretchDiameter :: NodeCount -> GraphWeights -> Tracer a (Double, Double)
treeStretchDiameter nodeCount weights = Tracer () \case
  Nothing -> return ()
  _ -> do
    Env tree <- ask
    frozen <- sendM $ freeze tree
    let stretchDiameter = avgTreeStretchDiameter nodeCount weights frozen
    output stretchDiameter

totalTreeWeight :: NodeCount -> GraphWeights -> Tracer a Double
totalTreeWeight n weights = Tracer () \case
  Nothing -> return ()
  _ -> do
    Env tree <- ask
    s <- sum <$> forM [0 .. n - 1] \a -> do
      b <- sendM $ readArray tree a
      return $ weights ! (a, b)
    output s

-- | Calculates the average tree stretch given the complete graph weights and a tree. The stretch for a pair of nodes (u, v) is the ratio of the shortest path in the tree over the shortest path in the complete graph (which is assumed to be euclidian, so the shortest path is always directly the edge (u, v)). The average tree stretch is the average stretch over all node pairs (u, v) with u != v. Complexity /O(n^2)/
avgTreeStretchDiameter :: NodeCount -> GraphWeights -> RootedTree -> (Double, Double)
avgTreeStretchDiameter n weights tree = (sum summed / fromIntegral (n * (n - 1)), maximum maxed) where

  (summed, maxed) = unzip [ summedTreeStretch root | root <- [0 .. n - 1] ]

  -- TODO: Trace adjacency map with successor changes, which should be faster than rebuilding it all the time
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
  adjacent node = IntMap.findWithDefault (error $ "No node " ++ show node ++ ", inconsistent rooted tree") node graph

  -- | Sums up all tree stretches from the given 'Node' to all others. Complexity /O(n)/
  summedTreeStretch :: Node -> (Double, Double)
  summedTreeStretch root = go root (adjacent root) 0 where
    go :: Node -> IntSet -> Double -> (Double, Double)
    go parent children baseWeight = result where
      result = IntSet.foldl' onChildren (if root == parent then baseWeight else baseWeight / weights ! (root, parent), baseWeight) children
      onChildren (acc, maxAcc) child = (acc + summed', max maxAcc maxed') where
        (summed', maxed') = go child children' (baseWeight + weight)
        weight = weights ! (parent, child)
        children' = IntSet.delete parent $ adjacent child
