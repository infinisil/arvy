{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Arvy.Tree where

import           Arvy.Weights
import           Control.Monad
import           Data.Array.MArray
import qualified Data.Heap         as H
import           Polysemy
import           Polysemy.Trace

type TreeState arr = arr Word (Maybe Word)

ring :: MArray arr (Maybe Word) m => Word -> m (TreeState arr)
ring count = newListArray (0, count - 1) (Nothing : fmap Just [0..])

type Edge = (Int, Int)
type MSTEntry = H.Entry Double Edge

-- | Calculates a minimum spanning tree for a complete graph with the given weights using a modified Prim's algorithm. /O(n^2)/.
mst :: MArray arr (Maybe Int) m => Int -> GraphWeights -> m (arr Int (Maybe Int))
mst count weights = do
  arr <- newArray (0, count - 1) Nothing
  forM_ (mstEdges count weights) $ \(x, y) ->
    writeArray arr y (Just x)
  return arr

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


-- | A tree state in a graph with nodes indices of type @i@. @w@ represents write access.
data SpanningTree i (m :: * -> *) a where
  GetSuccessor :: i -> SpanningTree i m (Maybe i)
  SetSuccessor :: i -> Maybe i -> SpanningTree i m ()

makeSem ''SpanningTree

runTree :: forall i m arr r a .
  ( Ix i
  , MArray arr (Maybe i) m
  , Member (Lift m) r)
  => arr i (Maybe i)
  -> Sem (SpanningTree i ': r) a
  -> Sem r a
runTree arr = interpret $ \case
  GetSuccessor i ->
    sendM @m $ readArray arr i
  SetSuccessor i s ->
    sendM @m $ writeArray arr i s


traceTree :: forall i r a . (Show i, Member (SpanningTree i) r, Member Trace r) => Sem r a -> Sem r a
traceTree = intercept @(SpanningTree i) $ \case
  GetSuccessor i -> do
    s <- getSuccessor i
    trace $ "Getting successor for " ++ show i ++ ", which is " ++ show s
    return s
  SetSuccessor i s -> do
    trace $ "Setting successor for " ++ show i ++ " to " ++ show s
    setSuccessor i s
