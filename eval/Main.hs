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

module Main where

import           Arvy.Algorithm.Arrow
import           Arvy.Algorithm.ConstantRing
import           Arvy.Algorithm.Ivy

import           Algebra.Graph.AdjacencyIntMap hiding (tree)
import           Data.Array.IArray
import           Data.Array.IO                 (IOArray)
import           Data.Array.MArray
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

main :: IO ()
main = runM $ do
  let count = 1000
  let numberOfRequests = 100000
  weights <- runRandomIO $ randomWeights count
  --let weights = shortestPathWeights (symmetricClosure (clique [ 0.. count - 1]))
  let tree = mst count weights
  mutableTree <- sendM @IO (thaw tree :: IO (IOArray Int (Maybe Int)))

  runTraceIO
    $ runIgnoringTrace
    $ runOutputAsTrace @(Int, Int)
    -- $ measureDistances weights
    $ runRandomIO
    $ randomRequests count numberOfRequests
    -- $ requestsWorst @IO numberOfRequests weights tree
    $ runArvyLocal @IO @IOArray count weights mutableTree ivy
