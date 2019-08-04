module Evaluation.Tree where

import           Arvy.Local

import           Data.Array.IO
import           Data.Array.Unboxed
import           Data.IntMap        (IntMap)
import qualified Data.IntMap        as IntMap
import           Data.IntSet        (IntSet)
import qualified Data.IntSet        as IntSet
import           Polysemy
import Prelude
import Pipes
import qualified Pipes.Prelude as P

treeStretchDiameter :: (MArray arr Node m, Member (Lift m) r) => NodeCount -> GraphWeights -> arr Node Node -> Pipe a (a, (Double, Double)) (Sem r) x
treeStretchDiameter nodeCount weights tree = P.mapM \event -> do
  frozen <- sendM $ freeze tree
  return $ (event, avgTreeStretchDiameter nodeCount weights frozen)

-- | Maps all array index/elem pairs to a number and sums them
sumMapAssocs :: (Num n, MArray a e m, Ix i) => ((i, e) -> n) -> a i e -> m n
sumMapAssocs f arr = sum . map f <$> getAssocs arr

totalTreeWeight :: (MArray arr Node m, Member (Lift m) r) => GraphWeights -> arr Node Node -> Pipe a (a, Double) (Sem r) x
totalTreeWeight weights tree = P.mapM \event -> do
  res <- sendM $ sumMapAssocs (weights !) tree
  return (event, res)

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
