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
import           Polysemy.Random
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Trace

import           Arvy.Algorithm
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
  weights <- runM $ runRandomIO $ randomWeights count
  requests <- runM $ runRandomIO $ randomRequests count 1000
  tree <- mst count weights :: IO (IOArray Int (Maybe Int))
  runM
    $ runTraceIO
    $ runOutputAsTrace @(Int, Int)
    $ runListInput requests
    $ runArvyLocal @IO @IOArray count weights tree ivy


  return ()
