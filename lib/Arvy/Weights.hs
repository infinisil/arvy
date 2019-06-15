{-# LANGUAGE TemplateHaskell     #-}
module Arvy.Weights
  ( module Arvy.Weights
  ) where

import           Algebra.Graph.AdjacencyIntMap
import           Arvy.Utils
import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Bifunctor                (first)
import           Data.Tuple                    (swap)
import           Polysemy
import           Polysemy.Random

type GraphWeights = UArray (Int, Int) Double

-- | An effect for providing access to weights from a current node to others
data LocalWeights (m :: * -> *) a where
  WeightTo :: Int -> LocalWeights m Double

makeSem ''LocalWeights

-- | Run local weights with all weights in a matrix and a current node index
runLocalWeights :: GraphWeights -> Int -> Sem (LocalWeights ': r) a -> Sem r a
runLocalWeights weights source = interpret $ \case
  WeightTo target -> return $ weights ! (source, target)



-- | Generate weights for all vertex pairs from an underlying incomplete graph by calculating the shortest path between them. The Floyd-Warshall algorithm is used to compute this, so complexity is O(n^3) with n being the number of edges, no additional space except the resulting weights itself is used. Edge weights in the underlying graph are always assumed to be 1. Use 'symmetricClosure' on the argument to force an undirected graph.
shortestPathWeights :: AdjacencyIntMap -> GraphWeights
shortestPathWeights graph = runSTUArray $ do
  let v = vertexCount graph
  arr <- newArray ((0, 0), (v - 1, v - 1)) infinity
  forM_ (vertexList graph) $ \x ->
    -- Catch invalid indices early. Could also implement automatix shifting down of
    -- indices to fill all holes, but this shouldn't happen anyways for normal usage
    if x >= v
    then error $ "shortestPathWeights: Graph has vertex with index " ++ show x
      ++ " which is above or equal the number of total vertices " ++ show v
      ++ ". Some vertex index under that is not present in the graph"
      ++ ", shift all node indices down to fill the range."
    else writeArray arr (x, x) 0

  forM_ (edgeList graph) $ \(a, b) ->
    writeArray arr (a, b) 1


  forM_ [0..v - 1] $ \k ->
    forM_ [0..v - 1] $ \i ->
      forM_ [0..v - 1] $ \j -> do
        ij <- readArray arr (i, j)
        ik <- readArray arr (i, k)
        kj <- readArray arr (k, j)
        let ikj = ik + kj
        when (ij > ikj) $
          writeArray arr (i, j) ikj

  return arr

-- | Generate weights for all vertex pairs from an underlying incomplete graph by calculating the shortest path between them. The Floyd-Warshall algorithm is used to compute this, so complexity is O(n^3) with n being the number of edges, no additional space except the resulting weights itself is used. Edge weights in the underlying graph are always assumed to be 1. Use 'symmetricClosure' on the argument to force an undirected graph.
shortestPathWeights' :: GraphWeights -> GraphWeights
shortestPathWeights' weights = runSTUArray $ do
  let (_, (v, _)) = bounds weights
  arr <- thaw weights

  forM_ [0..v] $ \k ->
    forM_ [0..v] $ \i ->
      forM_ [0..v] $ \j -> do
        ij <- readArray arr (i, j)
        ik <- readArray arr (i, k)
        kj <- readArray arr (k, j)
        let ikj = ik + kj
        when (ij > ikj) $
          writeArray arr (i, j) ikj

  return arr

-- | Generates a number of random 2D points with coordinates from 0.0 to 1.0
randomPoints :: Member Random r => Int -> Sem r (Array Int (Double, Double))
randomPoints count = listArray (0, count - 1) <$> go count where
  go :: Member Random r => Int -> Sem r [(Double, Double)]
  go 0 = return []
  go n = do
    pair <- (,) <$> random <*> random
    (pair:) <$> go (n - 1)

-- | Generates weights based on the euclidian distance for a number of given 2D points
euclidianWeights :: Array Int (Double, Double) -> GraphWeights
euclidianWeights points = array ((low, low), (high, high))
  [ ((i, j), if i == j then 0 else sqrt $ (x1 - x2) ^^ (2 :: Int) + (y1 - y2) ^^ (2 :: Int))
  | i <- [low..high]
  , let (x1, y1) = points ! i
  , j <- [low..high]
  , let (x2, y2) = points ! j
  ] where
  (low, high) = bounds points

-- TODO: USe a good distributions in the random-fu package instead
-- | Generates random weights in a graph. Weights from nodes to themselves are always 0 (aka the matrix' diagonal is 0), and weights are always the same in both edge directions (aka the matrix is symmetrical).
randomWeights :: Member Random r => Int -> Sem r GraphWeights
randomWeights count = do
  weights <- traverse randomForEdge
    -- All edges in one direction only
    [ (i, j)
    | i <- [0 .. count - 1]
    , j <- [i + 1 .. count - 1] ]

  return $ array
    ((0, 0), (count - 1, count - 1))
    (diagonal ++ -- The diagonals, aka the weights from a node to itself
    weights ++ -- One half of the weights, going in one edge direction
    map (first swap) weights) -- Node indices swapped around, going the other direction
  where
    -- | The diagonal of the weight matrix which is all 0
    diagonal :: [((Int, Int), Double)]
    diagonal = [ ((i, i), 0) | i <- [0 .. count - 1] ]

    -- | Generates a random weight and combines it with the given index in a tuple
    randomForEdge :: Member Random r => i -> Sem r (i, Double)
    randomForEdge x = (x,) <$> random

ringWeights :: Int -> GraphWeights
ringWeights n = shortestPathWeights (symmetricClosure (circuit [0..n-1]))
