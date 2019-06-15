module Arvy.Tree where

import           Arvy.Weights
import           Data.Array.Unboxed
import qualified Data.Heap          as H
import           Polysemy
import           Polysemy.Random
import           System.Random      (mkStdGen)
import           Algebra.Graph.AdjacencyIntMap

type TreeState arr = arr Int (Maybe Int)

data InitialTreeParameter = InitialTreeParameter
  { initialTreeName :: String
  , initialTreeGet  :: forall r . Member Random r => Int -> GraphWeights -> Sem r (Array Int (Maybe Int))
  }

instance Show InitialTreeParameter where
  show (InitialTreeParameter { .. }) = "Initial tree: " ++ initialTreeName ++ ", on an 8-ring: "
    ++ show (snd . run . runRandom (mkStdGen 0) $ initialTreeGet 8 (shortestPathWeights (symmetricClosure (circuit [0..7]))) )

ring' :: InitialTreeParameter
ring' = InitialTreeParameter
  { initialTreeName = "ring"
  , initialTreeGet = \count _weights -> return (listArray (0, count - 1) (Nothing : fmap Just [0..]))
  }

type Edge = (Int, Int)
type MSTEntry = H.Entry Double Edge

-- | Calculates a minimum spanning tree for a complete graph with the given weights using a modified Prim's algorithm. /O(n^2)/.
mst :: InitialTreeParameter
mst = InitialTreeParameter
  { initialTreeName = "minimum spanning tree"
  , initialTreeGet = get
  }
  where
    get count weights = return $ array (0, count - 1)
      ((0, Nothing) : fmap edgeToElem edges)
      where edges = mstEdges count weights

    edgeToElem :: Edge -> (Int, Maybe Int)
    edgeToElem (x, y) = (y, Just x)

semiCircles :: InitialTreeParameter
semiCircles = InitialTreeParameter
  { initialTreeName = "semi circles"
  , initialTreeGet = \count _ -> let h = count `div` 2 in return $ array (0, count - 1)
     ( [ (i, Just (i + 1)) | i <- [0 .. h - 1] ]
    ++ [ (h, Nothing) ]
    ++ [ (i, Just (i - 1)) | i <- [h + 1 .. count - 1] ] )
  }

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


