{-# LANGUAGE TupleSections #-}

module Parameters.Tree where

import           Arvy.Algorithm.Collection
import           Arvy.Local
import           Data.Array.Unboxed
import qualified Data.Heap                 as H
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Polysemy
import Polysemy.RandomFu
import Data.Random.Distribution.Uniform
import Utils
import Data.Tuple (swap)

-- | A representation of how an initial spanning tree and node states @s@ is generated
data InitialTreeParameter s r = InitialTreeParameter
  { initialTreeId :: String
  -- ^ The identifying string for this type of initial trees
  , initialTreeDescription :: String
  -- ^ The description of how this tree is generetade
  , initialTreeGet  :: NodeCount -> GraphWeights -> Sem r (RootedTree, Array Node s)
  -- ^ How to generate the tree and node states from a given number of nodes and their edge weights
  }

ring :: InitialTreeParameter () r
ring = InitialTreeParameter
  { initialTreeId = "ring"
  , initialTreeDescription = "Ring tree"
  , initialTreeGet = \n _ ->
      return ( listArray (0, n - 1) (0 : [0..])
             , listArray (0, n - 1) (replicate n ()))
  }

-- | Constructs a ring-like tree where the root is in the middle
semiCircles :: InitialTreeParameter RingNodeState r
semiCircles = InitialTreeParameter
  { initialTreeId = "semi"
  , initialTreeDescription = "Semi circles"
  , initialTreeGet = \n _ -> return ( tree n
                                    , listArray (0, n - 1) (replicate n SemiNode) // [ (n `div` 2 - 1, BridgeNode) ] )
  }
  where
    tree :: NodeCount -> RootedTree
    tree n = array (0, n - 1)
        ( [ (i, i + 1) | i <- [0 .. h - 1] ]
        ++ [ (h, h) ]
        ++ [ (i, i - 1) | i <- [h + 1 .. n - 1] ] )
      where
        h = n `div` 2

random :: Member RandomFu r => InitialTreeParameter () r
random = InitialTreeParameter
  { initialTreeId = "random"
  , initialTreeDescription = "Random spanning tree"
  , initialTreeGet = \n _ -> do
      tree <- randomSpanningTree n
      return ( tree
             , listArray (0, n - 1) (replicate n ()) )
  }

-- | Generates a uniform random spanning tree in a complete graph. Complexity /O(n * log(n))/
randomSpanningTree :: forall r . Member RandomFu r => NodeCount -> Sem r RootedTree
randomSpanningTree n = do
  -- Select a random root
  root <- sampleRVar (integralUniform 0 (n - 1))
  -- Connect all non-root nodes randomly to the root
  edges <- go (Set.singleton root) (Set.fromDistinctAscList ([0..root-1] ++ [root+1..n-1]))
  return $ array (0, n - 1) ((root, root) : edges)
  where
  -- | @go in ex@ randomly extends nodes @in@ included in the spanning tree randomly with nodes @ex@ not yet included. Outputs all necessary edges
  go :: Set Node -> Set Node -> Sem r [Edge]
  go included excluded
    | Set.size excluded == 0 = return []
    | otherwise = do
        -- Select a random node from the excluded set and one from the included set
        e <- sampleRVar (randomSetElement excluded)
        i <- sampleRVar (randomSetElement included)
        -- Recurse while inserting the previously-excluded node to the included ones and deleting it from the excluded ones
        ((e, i):) <$> go (Set.insert e included) (Set.delete e excluded)

-- | Calculates a minimum spanning tree for a complete graph with the given weights using a modified Prim's algorithm. 0 is always the root node. Complexity /O(n^2)/.
mst :: InitialTreeParameter () r
mst = InitialTreeParameter
  { initialTreeId = "mst"
  , initialTreeDescription = "Minimum spanning tree"
  , initialTreeGet = \n weights ->
      return ( array (0, n - 1) ((0, 0) : fmap swap (mstEdges n weights))
             , listArray (0, n - 1) (replicate n ()) )
  }

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
