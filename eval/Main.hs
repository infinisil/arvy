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

import           Arvy
import           Arvy.Algorithm
import           Arvy.Algorithm.Arrow
import           Arvy.Algorithm.Ivy
import           Arvy.Requests
import           Arvy.Tree
import           Arvy.Weights


testParams :: Parameters
testParams = Parameters
  { nodeCount = 1000
  , weights = randomWeights
  , initialTree = mst
  , requestCount = 10000
  , requests = randomRequests
  , algorithm = arrow
  }

main :: IO ()
main = runParams testParams
