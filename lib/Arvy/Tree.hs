{-# LANGUAGE BlockArguments #-}

module Arvy.Tree where

import           Arvy.Utils
import           Arvy.Weights
import           Data.Array.Unboxed
import qualified Data.Heap          as H
import qualified Data.Map           as M
import           Data.Maybe
import           Data.Array.ST
import qualified Data.Tree          as T
import           Control.Monad.ST
import           Control.Monad

-- TODO: Could be a lot faster with an UArray Node Node where a node pointing to itself represents a root node
-- | A rooted spanning tree, an array of nodes where each node either points to a successor signified with 'Just' or is the root node, signified with 'Nothing'
type RootedTree = Array Node (Maybe Node)

-- | Constructs a tree in the form of a ring. Node 0 is the root and node k points to k-1
ringTree :: NodeCount -> RootedTree
ringTree n = listArray (0, n - 1) (Nothing : fmap Just [0..])

-- | Calculates the average tree stretch given the complete graph weights and a tree. The stretch for a pair of nodes (u, v) is the ratio of the shortest path in the tree over the shortest path in the complete graph (which is assumed to be euclidian, so the shortest path is always directly the edge (u, v)). The average tree stretch is the average stretch over all node pairs (u, v) with u != v.
avgTreeStretch :: NodeCount -> GraphWeights -> RootedTree -> Double
avgTreeStretch n weights tree = runST go where
  go :: forall s . ST s Double
  go = do
    -- Create a new weight array that doesn't know any paths between any nodes
    treePaths :: GraphWeightsArr (STUArray s) <- newArray ((0, 0), (n - 1, n - 1)) infinity

    -- Insert all tree edges into the weights
    forM_ [0 .. n - 1] $ \src ->
      case tree ! src of
        Nothing -> return ()
        Just dst -> do
          let weight = weights ! (src, dst)
          writeArray treePaths (src, dst) weight
          writeArray treePaths (dst, src) weight

    -- Run floyd-warshall on it to find shortest paths between all nodes
    floydWarshall n treePaths

    -- Sum up the ratio of the path length in the tree over the path length in the complete graph for all nodes u, v with u < v
    summed <- sum <$> forM
      [ (u, v) | u <- [0 .. n - 1], v <- [u + 1 .. n - 1] ]
      \nodePair -> do
        treePath <- readArray treePaths nodePair
        return $ treePath / weights ! nodePair

    -- Divide by the number of nodes we summed up, which is `n * (n-1) / 2` when you do the math
    return $ summed / fromIntegral (n * (n - 1) `div` 2)

-- | Converts a rooted spanning tree in the form of a pointer array to a 'T.Tree' value, useful for processing or display with 'T.drawTree'.
-- Throws an error when there's multiple or no roots. Does *not* throw an error when some nodes don't transitively point to the root, instead those nodes are just not included in the final tree structure.
treeStructure :: RootedTree -> T.Tree Node
treeStructure tree = T.unfoldTree predecessors root where

  predecessors :: Node -> (Node, [Node])
  predecessors node = (node, M.findWithDefault [] node predecessorMap )

  (mroot, predecessorMap) = invert (assocs tree)
  root = fromMaybe (error "Tree has no root") mroot

  -- TODO: Use more efficient representation for predecessors, e.g. IntSet
  -- | Inverts an (node index, successor pointer) list to a (root, predecessor mapping) value
  invert :: [(Node, Maybe Node)] -> (Maybe Node, M.Map Node [Node])
  invert []                   = (Nothing, M.empty)
  invert ((i, pointer):rest) = case pointer of
    Nothing        -> (case root' of
                         Nothing -> Just i
                         Just i' -> error $ "Tree has multiple roots at both node " ++ show i ++ " and " ++ show i'
                      , rest')
    Just successor -> (root'
                      , M.insertWith (++) successor [i] rest')
    where (root', rest') = invert rest


-- | Calculates a minimum spanning tree for a complete graph with the given weights using a modified Prim's algorithm. 0 is always the root node. Complexity /O(n^2)/.
mst :: NodeCount -> GraphWeights -> RootedTree
mst n weights = array (0, n - 1)
  ((0, Nothing) : fmap edgeToElem edges)
  where
    edges = mstEdges n weights

    edgeToElem :: Edge -> (Node, Maybe Node)
    edgeToElem (x, y) = (y, Just x)

-- | Constructs a ring-like tree where the root is in the middle
semiCircles :: NodeCount -> RootedTree
semiCircles n = array (0, n - 1)
     ( [ (i, Just (i + 1)) | i <- [0 .. h - 1] ]
    ++ [ (h, Nothing) ]
    ++ [ (i, Just (i - 1)) | i <- [h + 1 .. n - 1] ] )
  where
    h = n `div` 2

type MSTEntry = H.Entry Double Edge

-- TODO: Implement with ST by using 2 mutable arrays, one with Double values, initially with weights to node 0, over time updated with shortest weights to other nodes. Second array with Int values representing the corresponding nodes with the shortest edge, initially all 0. The first array gets more infinity values over time, for every node that is included in the MST already. We get the minimum of it until we encounter infinity. When minimum found, update both arrays with updated edge weights and nodes.
-- | Calculates the edges of a minimum spanning tree for a complete graph with the given weights using a modified Prim's algorithm. 0 is always the root node. Complexity /O(n^2)/.
mstEdges :: NodeCount -> GraphWeights -> [Edge]
mstEdges n weights = go initialHeap where
  -- | Initial heap containing the weights from node 0 to every other node, so 0 is arbitrarily
  -- chosen as the initial node included in the MST. /O(n)/.
  initialHeap :: H.Heap MSTEntry
  initialHeap = H.fromList
    [ H.Entry (weights ! (0, x)) (0, x)
    | x <- [1 .. n - 1] ]

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


