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
module Tree where

import           Data.Array.MArray
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Data.Tree
import           Polysemy
import           Polysemy.Trace
import           Weights

type InitialTree = Tree Int

treeSet :: Tree Int -> Maybe (Set Int)
treeSet (Node n f) = do
  f' <- forestSet f
  if Set.member n f'
    then Nothing
    else return (Set.insert n f')
  where
    forestSet :: Forest Int -> Maybe (Set Int)
    forestSet [] = return Set.empty
    forestSet (x:xs) = do
      x' <- treeSet x
      xs' <- forestSet xs
      if Set.disjoint x' xs'
        then return (Set.union x' xs')
        else Nothing


treeToArray :: forall i arr m . (Ix i, Num i, MArray arr (Maybe i) m) => i -> Tree i -> m (arr i (Maybe i))
treeToArray count (Node n children) = do
  arr <- newArray (0, count) Nothing :: m (arr i (Maybe i))
  setChildren arr n children
  return arr
  where
    setChildren :: arr i (Maybe i) -> i -> [Tree i] -> m ()
    setChildren arr = go where
      go _ [] = return ()
      go parent (Node n children:xs) = do
        readArray arr n >>= \case
          Nothing -> return ()
          Just _ -> error ""
        writeArray arr n (Just parent)
        go n children
        go parent xs


ring :: Int -> GraphWeights -> Tree Int
ring nodeCount _ = ring' 0 (nodeCount - 1) where
  ring' :: Int -> Int -> Tree Int
  ring' n high = Node n children where
    children
      | n == high = []
      | otherwise = [ring' (n + 1) high]

mst :: Int -> GraphWeights -> Tree Int
mst = undefined


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


traceTree :: forall i w r a . (Show i, Member (SpanningTree i) r, Member Trace r) => Sem r a -> Sem r a
traceTree = intercept @(SpanningTree i) $ \case
  GetSuccessor i -> do
    s <- getSuccessor i
    trace $ "Getting successor for " ++ show i ++ ", which is " ++ show s
    return s
  SetSuccessor i s -> do
    trace $ "Setting successor for " ++ show i ++ " to " ++ show s
    setSuccessor i s
