{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}

module Arvy where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array         (Array)
import           Data.Array.IArray  (IArray, listArray, (!), (//))
import           Data.Array.ST
import           Data.Array.Unboxed (UArray)
import qualified Debug.Trace        as D
import           Polysemy
import           Polysemy.Output
import           Polysemy.State
import           Polysemy.Trace




data Algorithm r = forall m . Algorithm
  { sendRequest
    :: forall i . i -- | Current node index
    -> Sem r (m i)
  , forwardRequest
    :: forall i . m i -- ^ Incoming message
    -> i -- ^ Current node index
    -> Sem r (i, m i) -- ^ Return the next predecessor and message
  , recvRequest
    :: forall i . m i -- ^ Incoming message
    -> i -- ^ Current node index
    -> Sem r i -- ^ Return the next predecessor
  }

newtype ArrowMessage i = ArrowMessage i

arrow :: Algorithm r
arrow = Algorithm
  { sendRequest = return . ArrowMessage
  , forwardRequest = \(ArrowMessage sender) myself ->
      return (sender, ArrowMessage myself)
  , recvRequest = \(ArrowMessage sender) myself ->
      return sender
  }

newtype IvyMessage i = IvyMessage i

ivy :: Algorithm r
ivy = Algorithm
  { sendRequest = return . IvyMessage
  , forwardRequest = \(IvyMessage sender) myself ->
      return (sender, IvyMessage sender)
  , recvRequest = \(IvyMessage sender) myself ->
      return sender
  }

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

data STEffect s (m :: * -> *) a where
  DoST :: ST s a -> STEffect s m a

makeSem ''STEffect

test :: forall s r . Member (Lift (ST s)) r => STArray s Int Int -> Sem r Int
test arr = do
  x <- sendM @(ST s) $ readArray arr 0
  return x

runit :: IO [Double]
runit = do
  let (outputs, result) = runST runTest
  forM_ outputs putStrLn
  return result



runTest :: forall s . ST s ([String], [Double])
runTest = do
  let n = 100
  graphState <- initialTree n
  runM
    $ runFoldMapOutput (\i -> [i])
    $ runTraceAsOutput
    $ runAlgorithm @s (full n) graphState (take 1000 $ requests n) ivy


runAlgorithm :: forall s r . Members '[Trace, Lift (ST s)] r => GraphWeights -> GraphState s -> Requests -> Algorithm r -> Sem r [Double]
runAlgorithm weights tree requests algorithm = forM requests (processRequest @s tree algorithm) where
  processRequest :: forall s r . Members '[Trace, Lift (ST s)] r => GraphState s -> Algorithm r -> Int -> Sem r Double
  processRequest tree alg@Algorithm { .. } nodeIndex = do
    Node msucc <- sendM @(ST s) $ readArray tree nodeIndex
    case msucc of
      Nothing -> do
        trace $ "Node " ++ show nodeIndex ++ " already has the token"
        return 0
      Just succ -> do
        trace $ "Node " ++ show nodeIndex ++ " is sending a request towards " ++ show succ
        initMsg <- sendRequest nodeIndex
        let weight = weights ! (nodeIndex, succ)
        sendM @(ST s) $ writeArray tree nodeIndex (Node Nothing)
        restWeight <- sendMsg tree forwardRequest recvRequest initMsg succ

        return $ weight + restWeight
  sendMsg
    :: forall s r m . Members '[Trace, Lift (ST s)] r
    => GraphState s
    -> (m Int
      -> Int
      -> Sem r (Int, m Int))
    -> (m Int
      -> Int
      -> Sem r Int)
    -> m Int
    -> Int
    -> Sem r Double
  sendMsg tree forwardReq recvReq msg nodeIndex = do
    Node msucc <- sendM @(ST s) $ readArray tree nodeIndex
    case msucc of
      Nothing -> do
        trace $ "Node " ++ show nodeIndex ++ " received the request and has the token"
        next <- recvReq msg nodeIndex
        sendM @(ST s) $ writeArray tree nodeIndex (Node (Just next))
        return 0
      Just succ -> do
        trace $ "Node " ++ show nodeIndex ++ " received the request and will forward it to " ++ show succ
        (next, msg) <- forwardReq msg nodeIndex
        sendM @(ST s) $ writeArray tree nodeIndex (Node (Just next))
        let weight = weights ! (nodeIndex, succ)
        restWeight <- sendMsg tree forwardReq recvReq msg succ
        return $ weight + restWeight



main :: IO ()
main = putStrLn "Hello, Haskell!"
