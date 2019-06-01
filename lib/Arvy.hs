{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module Arvy where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.IArray  (IArray, listArray, (!), (//))
import           Data.Array.IO      (IOArray)
import qualified Data.Array.MArray  as M
import           Data.Array.ST
import           Data.Array.Unboxed (UArray)
import           Data.Monoid
import           Polysemy
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Trace

{- |
An Arvy algorithm, a combination between Arrow and Ivy.
@r@ stands for the effects it runs in, chosen by the caller, but the algorithm can enforce certain constraints on it, like requiring randomness.
@msg@ stands for the message type it sends between nodes, this can be arbitrarily chosen by the algorithm.
@i@ stands for the node index type, which can be passed via messages and be has to be used to select which node to choose as the next successor.

- When a node @x@ initiates a request, 'arvyInitiate' is called to generate the initial message, which has access to the current node's index through a 'Reader i' effect.
- This message gets passed along the path from @x@ to the current token holder @y@. For nodes the request travels *through*, 'arvyTransmit' is called, which can transform the message, potentially adding a reference to the current node's index.
- Whenever a message arrives at a node, 'arvySelect' is called, which has to select a node index from the received message. For this selection, the algorithm does *not* have access to the current node's index. This is to ensure correctness of it, because only *previously* traversed nodes should be selected.

-}
data Arvy r = forall msg n . Arvy
  { arvyNodeInit :: forall i . Sem (Reader i ': r) n
  , arvyInitiate :: forall i . Sem (State n ': Reader i ': r) (msg i)
  , arvyTransmit :: forall i . msg i -> Sem (State n ': Reader i ': r) (msg i)
  , arvySelect   :: forall i . msg i -> Sem (State n ': r) i
  }

fromSelection :: (forall i . [i] -> Sem (State () ': r) i) -> Arvy r
fromSelection select = Arvy
  { arvyNodeInit = return ()
  , arvyInitiate = do
      i <- ask
      return [i]
  , arvyTransmit = \msg -> do
      i <- ask
      return $ i : msg
  , arvySelect = select
  }

arrow' :: Arvy r
arrow' = fromSelection (return . head)

ivy' :: Arvy r
ivy' = fromSelection (return . last)

data Tree i (m :: * -> *) a where
  GetSuccessor :: i -> Tree i m (Maybe i)
  SetSuccessor :: i -> Maybe i -> Tree i m ()

makeSem ''Tree

runTree :: forall i m arr r a .
  ( Ix i
  , M.MArray arr (Maybe i) m
  , Member (Lift m) r)
  => arr i (Maybe i)
  -> Sem (Tree i ': r) a
  -> Sem r a
runTree arr = interpret $ \case
  GetSuccessor i ->
    sendM @m $ readArray arr i
  SetSuccessor i s ->
    sendM @m $ writeArray arr i s

doTest :: IO ()
doTest = do
  x <- newListArray (0, 5) [Nothing, Just 0, Just 1, Just 2, Just 3] :: IO (IOArray Int (Maybe Int))
  dist <- runM
    $ runTraceIO
    $ runTree @Int @IO x
    $ traceTree @Int
    $ forM (take 10 $ cycle [0..4]) (
      measureDistances weights
      . traceMessages @Int
      . runArvyLocal @Int arrow
    )
  print dist
  where weights = full 5

traceMessages :: forall i r a . (Member Trace r, Member (Output (i, i)) r, Show i) => Sem r a -> Sem r a
traceMessages = intercept @(Output (i, i)) $ \case
  Output (from, to) -> do
    trace $ show from ++ " -> " ++ show to
    output (from, to)

runArvyLocal :: Members '[Trace, Tree i, Output (i, i)] r => Arvy r -> i -> Sem r ()
runArvyLocal (Arvy n s t rr) = runArvyLocal' (n, s, t, rr) where
  runArvyLocal' :: forall i r msg n
    . Members '[Trace, Tree i, Output (i, i)] r
    => ( Sem (Reader i ': r) n
       , Sem (State n ': Reader i ': r) (msg i)
       , msg i -> Sem (State n ': Reader i ': r) (msg i)
       , msg i -> Sem (State n ': r) i
       )
    -> i
    -> Sem r ()
  runArvyLocal' (nodeInit, send, transfer, select) r = do
    trace "Initiating request"
    getSuccessor r >>= \case
      Nothing -> trace "Token is already here\n"
      Just n -> do
        setSuccessor r Nothing
        (_, msg) <- runReader r (runState undefined send)
        output (r, n)
        go msg n
    where
      go :: msg i -> i -> Sem r ()
      go msg r = do
        (_, nextSucc) <- runState undefined $ select msg
        currentSucc <- getSuccessor r
        setSuccessor r (Just nextSucc)
        case currentSucc of
          Nothing ->
            trace "Request arrived at token holder\n"
          Just n -> do
            (_, newMsg) <- runReader r $ runState undefined $ transfer msg
            output (r, n)
            go newMsg n

-- | Measures distances
measureDistances :: (Ix i, IArray arr n, Num n) => arr i n -> Sem (Output i ': r) a -> Sem r (Sum n, a)
measureDistances weights = runFoldMapOutput (Sum . (weights !))

traceTree :: forall i r a . (Show i, Member (Tree i) r, Member Trace r) => Sem r a -> Sem r a
traceTree = intercept @(Tree i) $ \case
  GetSuccessor i -> do
    s <- getSuccessor i
    trace $ "Getting successor for " ++ show i ++ ", which is " ++ show s
    return s
  SetSuccessor i s -> do
    trace $ "Setting successor for " ++ show i ++ " to " ++ show s
    setSuccessor i s


newtype ArrowMessage i = ArrowMessage i

arrow :: Arvy r
arrow = Arvy
  { arvyNodeInit = return ()
  , arvyInitiate =
      ArrowMessage <$> ask
  , arvyTransmit = \_ ->
      ArrowMessage <$> ask
  , arvySelect = \(ArrowMessage sender) ->
      return sender
  }

newtype IvyMessage i = IvyMessage i

ivy :: Arvy r
ivy = Arvy
  { arvyNodeInit = return ()
  , arvyInitiate =
      IvyMessage <$> ask
  , arvyTransmit =
      return
  , arvySelect = \(IvyMessage root) ->
      return root
  }


-- | TODO: https://hackage.haskell.org/package/repa seems like a better fit for this, because it supports taking slices
type GraphWeights = UArray (Int, Int) Double

-- | TODO: Implement https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm for https://hackage.haskell.org/package/algebraic-graphs, then use that to generate the weights
full :: Int -> GraphWeights
full n = listArray ((0, 0), (n - 1, n - 1)) (repeat 1) // [ ((i, i), 0) | i <- [0..(n - 1)] ]

type Requests = [Int]

requests :: Int -> Requests
requests n = iterate (\i -> (i + 1) `mod` n) 0

initialTree :: Int -> ST s (GraphState s)
initialTree n = do
  arr <- newArray (0, n - 1) (Node Nothing)
  forM_ [1..n-1] $ \i ->
    writeArray arr i (Node (Just (i - 1)))
  return arr

type NodeIndex = Int

newtype Node = Node
  { successor :: Maybe NodeIndex
  } deriving Show

type GraphState s = STArray s Int Node
