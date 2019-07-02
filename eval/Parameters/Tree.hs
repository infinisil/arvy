module Parameters.Tree where

import           Arvy.Local
import           Data.Array.Unboxed
import qualified Data.Heap          as H
import           Polysemy

data InitialTreeParameter r = InitialTreeParameter
  { initialTreeName :: String
  , initialTreeGet  :: Int -> GraphWeights -> Sem r (Array Int (Maybe Int))
  }

ring :: InitialTreeParameter r
ring = InitialTreeParameter
  { initialTreeName = "ring"
  , initialTreeGet = \n _ ->
      return (listArray (0, n - 1) (Nothing : fmap Just [0..]))
  }



-- | Constructs a ring-like tree where the root is in the middle
semiCircles :: InitialTreeParameter r
semiCircles = InitialTreeParameter
  { initialTreeName = "semi circles"
  , initialTreeGet = \n _ -> return (tree n)
  }
  where
    tree :: NodeCount -> RootedTree
    tree n = array (0, n - 1)
        ( [ (i, Just (i + 1)) | i <- [0 .. h - 1] ]
        ++ [ (h, Nothing) ]
        ++ [ (i, Just (i - 1)) | i <- [h + 1 .. n - 1] ] )
      where
        h = n `div` 2


-- | Calculates a minimum spanning tree for a complete graph with the given weights using a modified Prim's algorithm. 0 is always the root node. Complexity /O(n^2)/.
mst :: InitialTreeParameter r
mst = InitialTreeParameter
  { initialTreeName = "mst"
  , initialTreeGet = \n weights ->
      return $ array (0, n - 1) ((0, Nothing) : fmap edgeToElem (mstEdges n weights))
  }
  where
    edgeToElem :: Edge -> (Node, Maybe Node)
    edgeToElem (x, y) = (y, Just x)

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
