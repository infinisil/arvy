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
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module Arvy where

import           Algebra.Graph.AdjacencyIntMap
import           Data.Array.IArray
import           Data.Array.IO                 (IOArray)
import           Data.Monoid
import           Polysemy
import           Polysemy.Output
import           Polysemy.Random
import           Polysemy.Trace

import           Arvy.Algorithm
import           Arvy.Algorithm.Arrow
import           Arvy.Algorithm.Ivy
import           Arvy.Requests
import           Arvy.Tree
import           Arvy.Weights


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


main :: IO ()
main = do
  let count = 100
  let numberOfRequests = 10000
  -- weights <- runM $ runRandomIO $ randomWeights count
  let weights = shortestPathWeights (symmetricClosure (clique [ 0.. count - 1]))
  tree <- mst count weights :: IO (IOArray Int (Maybe Int))

  (Sum dist, _) <- runM
    $ runIgnoringTrace
    -- $ runOutputAsTrace @(Int, Int)
    $ measureDistances weights
    $ runRandomIO
    $ randomRequests count numberOfRequests
    -- $ requestsWorst @IO numberOfRequests weights tree
    $ runArvyLocal @IO @IOArray count weights tree ivy

  print dist
  return ()
