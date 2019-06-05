{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE BlockArguments            #-}
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

import           Algebra.Graph.Class
import           Control.Monad
import           Data.Array.IArray
import           Data.Array.IO       (IOArray)
import qualified Data.Array.MArray   as M
import           Data.Array.ST
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Trace

import           Algorithm
import           Tree
import           Weights

-- | Interpret a 'State' with a mutable array element, where a 'Reader' gives the index.
runStateMArray :: forall i arr n m r a .
  ( Ix i
  , M.MArray arr n m
  , Member (Reader i) r
  , Member (Lift m) r
  )
  => arr i n
  -> Sem (State n ': r) a
  -> Sem r a
runStateMArray arr = interpret $ \case
  Get -> do
    i <- ask
    sendM @m $ readArray arr i
  Put value -> do
    i <- ask
    sendM @m $ writeArray arr i value


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
  where weights = shortestPathWeights (circuit [0..4])

traceMessages :: forall i r a . (Member Trace r, Member (Output (i, i)) r, Show i) => Sem r a -> Sem r a
traceMessages = intercept @(Output (i, i)) $ \case
  Output (from, to) -> do
    trace $ show from ++ " -> " ++ show to
    output (from, to)

-- | Measures distances
measureDistances :: (Ix i, IArray arr n, Num n) => arr i n -> Sem (Output i ': r) a -> Sem r (Sum n, a)
measureDistances weights = runFoldMapOutput (Sum . (weights !))

requests :: Int -> [Int]
requests n = iterate (\i -> (i + 1) `mod` n) 0

type NodeIndex = Int

newtype Node = Node
  { successor :: Maybe NodeIndex
  } deriving Show

type GraphState s = STArray s Int Node

{- |

- Graph node count, any Int, arbitrary choice
- Graph weights, UArray (Int, Int) Double, depends on graph node count, multiple possibilities
- Initial tree state, (rose) Tree, depends on node count and may depend on graph weights, multiple possibilities
- Algorithm, multiple choices
- Request sequence

-}


-- ######### Node count #########
type NodeCount = Int



-- ########## Request sequence ##########

{- |

What should the request sequence have access to?
- Node count
- Graph weights
- Current algorithm tree

Does not have access to
- Algorithms RNG
- Node-local state (why not?)
-}

