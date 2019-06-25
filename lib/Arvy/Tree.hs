{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BangPatterns #-}

module Arvy.Tree where

import           Arvy.Utils
import           Arvy.Weights
import           Data.Array.Unboxed
import qualified Data.Heap          as H
import qualified Data.Map           as M
import           Data.Maybe
import qualified Data.Tree          as T
import Polysemy
import Control.Exception
import Data.Array.MArray
import Data.Array.Base
import Control.Monad.ST
import Control.Monad

type Tree = Array Int (Maybe Int)

ringTree :: Int -> Array Int (Maybe Int)
ringTree count = listArray (0, count - 1) (Nothing : fmap Just [0..])

avgTreeStretch' :: Int -> GraphWeights -> Tree -> Double
avgTreeStretch' n weights tree = sum stretches / (fromIntegral n * (fromIntegral n - 1) / 2)
  where

    stretches = [ stretch u v | u <- [0 .. n - 1], v <- [u + 1 .. n - 1] ]

    stretch :: Int -> Int -> Double
    stretch u v = treeDistances ! (u, v) / weights ! (u, v)

    w = accumArray (flip const) infinity ((0, 0), (n - 1, n - 1))
      (concat [ [ ((x, y), weight)
                , ((y, x), weight) ]
      | (x, Just y) <- assocs tree
      , let weight = weights ! (x, y)
      ])
    treeDistances = shortestPathWeights' w

avgTreeStretch :: Int -> GraphWeights -> Tree -> Double
avgTreeStretch n weights tree = runST go where
  go :: forall s . ST s Double
  go = do
    arr :: STUArray s (Int, Int) Double <- newArray ((0, 0), (n - 1, n - 1)) infinity
    let (lower, upper) = bounds tree
    forM_ [lower..upper] $ \src -> do
      let mdst = tree ! src
      case mdst of
        Nothing -> return ()
        Just dst -> do
          let weight = weights ! (src, dst)
          writeArray arr (src, dst) weight
          writeArray arr (dst, src) weight

    forM_ [0 .. n - 1] $ \k ->
      forM_ [0 .. n - 1] $ \i ->
        forM_ [0 .. n - 1] $ \j -> do
          ij <- unsafeRead arr (i * n + j)
          ik <- unsafeRead arr (i * n + k)
          kj <- unsafeRead arr (k * n + j)
          --let ikj = {-# SCC letd #-} ik + kj
          when (ij > ik + kj) $
            unsafeWrite arr (i * n + j) (ik + kj)

    res <- foldM (\acc u ->
              foldM (\acc' v -> do
                        treePath <- readArray arr (u, v)
                        return $ acc' + treePath / weights ! (u, v)
                    ) acc [u + 1 .. n - 1]
          ) 0 [0 .. n - 1]
    return $ res / (fromIntegral n * (fromIntegral n - 1) / 2)

-- | Converts a rooted spanning tree in the form of a pointer array to a 'T.Tree' value, useful for processing or display with 'T.drawTree'.
-- Throws an error when there's multiple or no roots. Does *not* throw an error when some nodes don't transitively point to the root, instead those nodes are just not included in the final tree structure.
treeStructure :: Array Int (Maybe Int) -> T.Tree Int
treeStructure tree = T.unfoldTree predecessors root where

  predecessors :: Int -> (Int, [Int])
  predecessors node = (node, M.findWithDefault [] node predecessorMap )

  (mroot, predecessorMap) = invert (assocs tree)
  root = fromMaybe (error "Tree has no root") mroot

  -- | Inverts an (node index, successor pointer) list to a (root, predecessor mapping) value
  invert :: [(Int, Maybe Int)] -> (Maybe Int, M.Map Int [Int])
  invert []                   = (Nothing, M.empty)
  invert ((i, pointer):rest) = case pointer of
    Nothing        -> (case root' of
                         Nothing -> Just i
                         Just i' -> error $ "Tree has multiple roots at both node " ++ show i ++ " and " ++ show i'
                      , rest')
    Just successor -> (root'
                      , M.insertWith (++) successor [i] rest')
    where (root', rest') = invert rest

type Edge = (Int, Int)
type MSTEntry = H.Entry Double Edge

-- | Calculates a minimum spanning tree for a complete graph with the given weights using a modified Prim's algorithm. /O(n^2)/.
mst :: Int -> GraphWeights -> Array Int (Maybe Int)
mst count weights = array (0, count - 1)
  ((0, Nothing) : fmap edgeToElem edges)
  where
    edges = mstEdges count weights

    edgeToElem :: Edge -> (Int, Maybe Int)
    edgeToElem (x, y) = (y, Just x)

semiCircles :: Int -> Array Int (Maybe Int)
semiCircles count = array (0, count - 1)
     ( [ (i, Just (i + 1)) | i <- [0 .. h - 1] ]
    ++ [ (h, Nothing) ]
    ++ [ (i, Just (i - 1)) | i <- [h + 1 .. count - 1] ] )
  where
    h = count `div` 2


-- | Initially, we have
-- p(v i ) = v i+1 for 1 ≤ i < n 2 , p(v i ) = v i−1 for n 2 < i ≤ n and
-- p(v n/2 ) = v n/2

-- TODO: Implement with ST by using 2 mutable arrays, one with Double values, initially with weights to node 0, over time updated with shortest weights to other nodes. Second array with Int values representing the corresponding nodes with the shortest edge, initially all 0. The first array gets more infinity values over time, for every node that is included in the MST already. We get the minimum of it until we encounter infinity. When minimum found, update both arrays with updated edge weights and nodes.
-- | Calculates the edges of a minimum spanning tree for a complete graph with the given weights using a modified Prim's algorithm. /O(n^2)/.
mstEdges :: Int -> GraphWeights -> [Edge]
mstEdges count weights = go initialHeap where
  -- | Initial heap containing the weights from node 0 to every other node, so 0 is arbitrarily
  -- chosen as the initial node included in the MST. /O(n)/.
  initialHeap :: H.Heap MSTEntry
  initialHeap = H.fromList
    [ H.Entry (weights ! (0, x)) (0, x)
    | x <- [1 .. count - 1] ]

  -- | Calculates the edges of a minimum spanning tree by repeatedly taking the minimum edge in the heap and updating the heap with new weights. /O(n^2)/
  go :: H.Heap MSTEntry -> [Edge]
  go heap = case H.viewMin heap of
    Nothing                            -> []
    Just (H.Entry _ edge@(_, y), rest) -> edge : go (H.map (updateWeight y) rest)

  -- | Updates the minimum weight to a node by checking if the new node in the MST has a
  -- smaller weight to it than all others. /O(1)/
  updateWeight :: Int -> MSTEntry -> MSTEntry
  updateWeight new entry@(H.Entry weight (_, dst))
    | newWeight < weight = H.Entry newWeight (new, dst)
    | otherwise = entry
    where newWeight = weights ! (new, dst)


