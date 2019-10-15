{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Parameters.Tree where

import Arvy.Algorithm
import Evaluation.Types
import           Data.Array.Unboxed
import           Data.Array.ST
import Control.Monad.ST
import qualified Data.Heap                 as H
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet as IntSet
import           Polysemy
import Polysemy.RandomFu
import Data.Random.Distribution.Uniform
import Utils
import Data.Tuple (swap)
import Control.Monad
import Data.Sequence (Seq, Seq(..))
import qualified Data.Sequence as Seq
import Data.Foldable (foldr')
import Data.STRef
import Data.MonoTraversable
import Data.Ord
import Data.Text (Text)
import Arvy.Log

data TreeParam r = TreeParam
  { treeName :: Text
  , treeGen :: NodeCount -> GraphWeights -> Sem r RootedTree
  }

ring :: TreeParam r
ring = TreeParam
  { treeName = "ring"
  , treeGen = \n _ ->
      return ( listArray (0, n - 1) (0 : [0..]) )
  }


random :: Member RandomFu r => TreeParam r
random = TreeParam
  { treeName = "random"
  , treeGen = \n _ -> randomSpanningTree n
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

shortPairs :: TreeParam r
shortPairs = TreeParam
  { treeName = "shortpairs"
  , treeGen = \n w ->
      return ( shortPairDistances n w 0 )
  }

shortPairDistances :: NodeCount -> GraphWeights -> Node -> RootedTree
shortPairDistances n weights root = runST $ do
  distArr <- newArray ((0, 0), (n - 1, n - 1)) 0
  nodeArr <- newArray (0, n - 1) 0
  edges <- go distArr nodeArr
  return $ array (0, n - 1) ((root, root) : edges)
  where

  go :: forall s . STUArray s Edge Double -> STUArray s Node Double -> ST s [Edge]
  go dists nodes = step (IntSet.singleton root) (IntSet.delete root (IntSet.fromDistinctAscList [0..n-1]))
    where
    step :: IntSet -> IntSet -> ST s [Edge]
    step included excluded
      | IntSet.size excluded == 0 = return []
      | otherwise = do
          let excluded' = IntSet.elems excluded
              included' = IntSet.elems included
              includedCount = fromIntegral $ IntSet.size included
          (old, new, _) <- foldM (\acc@(_, _, cost) candidateNew -> do
                                    (candidateOld, candidateCost) <- getBestNode candidateNew included' includedCount
                                    return $ if candidateCost < cost
                                      then (candidateOld, candidateNew, candidateCost)
                                      else acc
                                ) (-1, -1, infinity) excluded'
          update new old includedCount
          rest <- step (IntSet.insert new included) (IntSet.delete new excluded)
          return $ (new, old) : rest

    getBestNode :: Node -> [Node] -> Double -> ST s (Node, Double)
    getBestNode newNode included includedCount = do
      let f :: (Node, Double) -> Node -> ST s (Node, Double)
          f acc@(_, bestCost) otherNode = do
            otherValue <- readArray nodes otherNode
            let otherCost = includedCount * weights ! (newNode, otherNode) + otherValue
            return $ if otherCost < bestCost
              then (otherNode, otherCost)
              else acc
      foldM f (-1, infinity) included

    update :: Node -> Node -> Double -> ST s ()
    update newNode bestNode includedCount = do
      let bestWeight = weights ! (newNode, bestNode)

      forM_ [0..newNode-1] $ \i -> do
        dist <- (+bestWeight) <$> readArray dists (bestNode, i)
        writeArray dists (newNode, i) dist
        writeArray dists (i, newNode) dist

      bestValue <- readArray nodes bestNode
      writeArray nodes newNode (bestValue + includedCount * bestWeight)
      forM_ [0..newNode-1] $ \i -> do
        dist <- readArray dists (bestNode, i)
        prev <- readArray nodes i
        let new = prev + bestWeight + dist
        writeArray nodes i new

-- | Calculates a minimum spanning tree for a complete graph with the given weights using a modified Prim's algorithm. 0 is always the root node. Complexity /O(n^2)/.
mst :: TreeParam r
mst = TreeParam
  { treeName = "mst"
  , treeGen = \n weights ->
      return ( array (0, n - 1) ((0, 0) : fmap swap (mstEdges n weights)) )
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

-- | A Prufer code is a sequence of n - 2 numbers from [0..n - 1], all of which can be mapped one-to-one to spanning trees in a complete graph with n nodes
newtype PruferCode = PruferCode [Int] deriving Show

-- | Generates all Prufer codes for a specific n
allPruferCodes :: NodeCount -> [PruferCode]
allPruferCodes n
  | n < 2 = error "Prufer codes don't exist for n < 2"
  | otherwise = go n
  where
  go :: Int -> [PruferCode]
  go 2 = [ PruferCode [] ]
  go k = [ PruferCode (h : t) | h <- [0 .. n - 1], PruferCode t <- go (k - 1) ]

-- | Turns a Prufer code into the spanning tree it represents as a list of edges
pruferToTree :: NodeCount -> PruferCode -> [Edge]
pruferToTree n (PruferCode ps) = edges ps (frequencies ps) where
  frequencies :: [Int] -> Seq Int
  frequencies = foldr' (Seq.adjust' (+1)) (Seq.replicate n 1)

  edges :: [Int] -> Seq Int -> [Edge]
  edges (x:xs) s@(Seq.elemIndexL 1 -> Just y) = (x, y) : edges xs s' where
    s' = Seq.adjust' (subtract 1) x . Seq.adjust' (subtract 1) y $ s
  edges [] (Seq.elemIndicesL 1 -> [u, v]) = [(u, v)]
  edges _ _ = error "This case will never happen"


-- | Computes half the total pair distances in a tree for a given Prufer code and certain weights. Bails out early if the result would be bigger than the given @maxScore@
pruferHalfPairDistances :: NodeCount -> PruferCode -> GraphWeights -> Double -> Maybe Double
pruferHalfPairDistances n p weights maxScore = runST $ do
  distArr <- newArray ((0, 0), (n - 1, n - 1)) 0
  resultRef <- newSTRef 0

  underMax <- go resultRef distArr
  if underMax then Just <$> readSTRef resultRef
  else return Nothing

  where
    edges@((root,_):_) = reverse $ pruferToTree n p

    go :: forall s . STRef s Double -> STUArray s Edge Double -> ST s Bool
    go result dists = go' edges (IntSet.singleton root) where
      go' :: [Edge] -> IntSet -> ST s Bool
      go' [] _ = return True
      go' ((old, new):es) included = do
        let weight = weights ! (old, new)
        oforM_ included $ \i -> do
          dist <- readArray dists (i, old)
          let newDist = dist + weight
          writeArray dists (i, new) newDist
          writeArray dists (new, i) newDist
          modifySTRef' result (+newDist)

        score <- readSTRef result
        if score > maxScore then return False
        else go' es (IntSet.insert new included)


-- | Returns the edges of a spanning tree with minimum total pair distance. Complexity /O(n^n)/
bestPairDistanceTree :: forall r . LogMember r => NodeCount -> GraphWeights -> Sem r [Edge]
bestPairDistanceTree n weights = do
  cands <- candidatesForMaxScore infinity (allPruferCodes n) 0
  return $ pruferToTree n (last cands)
  where
  count = n ^ (n - 2)
  candidatesForMaxScore :: Double -> [PruferCode] -> Int -> Sem r [PruferCode]
  candidatesForMaxScore _ [] _ = return []
  candidatesForMaxScore maxScore (x:xs) k = case pruferHalfPairDistances n x weights maxScore of
    Just score -> do
      lgDebug $ percent <> "New half score: " <> tshow score <> " with " <> tshow (reverse $ pruferToTree n x)
      rest <- candidatesForMaxScore score xs (k + 1)
      return (x : rest)
    Nothing -> do
      when (k `mod` 500000 == 0) $
        lgDebug $ percent <> "No better score yet"
      candidatesForMaxScore maxScore xs (k + 1)
    where percent = "[" <> tshow (round $ (100 :: Double) / fromIntegral count * fromIntegral k :: Int) <> "] "

shortestPairs :: LogMember r => TreeParam r
shortestPairs = TreeParam
  { treeName = "shortestpairs"
  , treeGen = \n w -> reverse <$> bestPairDistanceTree n w >>= \case
      edges@((root, _):_) -> return ( array (0, n - 1) ((root, root) : edges) )
      _ -> error "No root! Shouldn't occur"
  }

bestStar :: TreeParam r
bestStar = TreeParam
  { treeName = "star"
  , treeGen = \n w -> do
      let center = bestStarCenter n w
      return ( array (0, n - 1) ((center, center) : map (,center) ([ 0 .. center - 1 ] ++ [ center + 1 .. n - 1 ] )) )
  } where
  bestStarCenter :: NodeCount -> GraphWeights -> Node
  bestStarCenter n weights = fst $ minimumByEx (comparing snd) (map (\i -> (i, starScore n weights i)) [0 .. n - 1])

  starScore :: NodeCount -> GraphWeights -> Node -> Double
  starScore n weights center = sum [ weights ! (center, i) | i <- [0 .. n - 1] ]
