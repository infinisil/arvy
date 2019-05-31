{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
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
import           Data.Array         (Array)
import           Data.Array.IArray  (IArray, listArray, (!), (//))
import           Data.Array.IO      (IOArray)
import qualified Data.Array.MArray  as M
import           Data.Array.ST
import           Data.Array.Unboxed (UArray)
import           Data.Monoid
import qualified Debug.Trace        as D
import           GHC.Generics
import           Polysemy
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Trace


data AlgorithmI r msg i = AlgorithmI
  { send     :: Sem (Reader i ': r) (msg i)
  , transfer :: msg i -> Sem (Reader i ': r) (i, msg i)
  , recv     :: msg i -> Sem (Reader i ': r) i
  }

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
  SetSuccessor i succ ->
    sendM @m $ writeArray arr i succ

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

runArvyLocal :: Members '[Trace, Tree i, Output (i, i)] r => Algorithm r -> i -> Sem r ()
runArvyLocal (Algorithm algi) = runArvyLocal' algi where
  runArvyLocal' :: forall i r msg . Members '[Trace, Tree i, Output (i, i)] r => AlgorithmI r msg i -> i -> Sem r ()
  runArvyLocal' AlgorithmI { send, recv, transfer } r = do
    trace "Initiating request"
    getSuccessor r >>= \case
      Nothing -> trace "Token is already here\n"
      Just n -> do
        msg <- runReader r send
        setSuccessor r Nothing
        output (r, n)
        go msg n
        where
          go :: msg i -> i -> Sem r ()
          go msg r = getSuccessor r >>= \case
            Nothing -> do
              res <- runReader r $ recv msg
              trace $ "Request arrived at token holder\n"
              setSuccessor r (Just res)
            Just n -> do
              (i, m) <- runReader r $ transfer msg
              setSuccessor r (Just i)
              output (r, n)
              go m n

-- | Measures distances
measureDistances :: (Ix i, IArray arr n, Num n) => arr i n -> Sem (Output i ': r) a -> Sem r (Sum n, a)
measureDistances weights = runFoldMapOutput (Sum . (weights !))

traceTree :: forall i r a . (Show i, Member (Tree i) r, Member Trace r) => Sem r a -> Sem r a
traceTree = intercept @(Tree i) $ \case
  GetSuccessor i -> do
    succ <- getSuccessor i
    trace $ "Getting successor for " ++ show i ++ ", which is " ++ show succ
    return succ
  SetSuccessor i succ -> do
    trace $ "Setting successor for " ++ show i ++ " to " ++ show succ
    setSuccessor i succ

data Algorithm r = forall m . Algorithm (forall i . AlgorithmI r m i)

newtype ArrowMessage i = ArrowMessage i

arrow :: Algorithm r
arrow = Algorithm (AlgorithmI
  { send = do
      i <- ask
      return (ArrowMessage i)
  , transfer = \(ArrowMessage sender) -> do
      i <- ask
      return (sender, ArrowMessage i)
  , recv = \(ArrowMessage sender) ->
      return sender
  })

newtype IvyMessage i = IvyMessage i

ivy :: Algorithm r
ivy = Algorithm (AlgorithmI
  { send = do
      i <- ask
      return (IvyMessage i)
  , transfer = \(IvyMessage root) -> do
      return (root, IvyMessage root)
  , recv = \(IvyMessage root) ->
      return root
  })

data AlgPart i a where
  Init :: AlgPart i (msg i)
  Transmit :: msg i -> AlgPart i (i, msg i)
  Recv :: msg i -> AlgPart i i

data Event a where
  MakeRequest :: Event ()
  ForwardRequest :: msg -> Event msg
  RecvRequest :: msg -> Event ()

handleEvent :: Event a -> a
handleEvent MakeRequest          = ()
handleEvent (ForwardRequest msg) = msg
handleEvent (RecvRequest msg)    = ()

type NewAlg r = forall a i . AlgPart i a -> Sem (Reader i ': r) a


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

{-

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

main :: IO ()
main = putStrLn "Hello, Haskell!"
-}
