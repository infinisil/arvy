{-# LANGUAGE TypeApplications #-}

module Main where

import           Arvy.Algorithm
import           Arvy.Algorithm.Ivy
import           Data.Array.IO      (IOArray)
import           Polysemy

import           Polysemy.Input
import           Polysemy.Random
import           Polysemy.Trace

import           Arvy.Requests
import           Arvy.Tree
import           Arvy.Weights

main :: IO ()
main = do
  let count = 1000
  weights <- runM $ runRandomIO $ randomWeights count
  requests <- runM $ runRandomIO $ randomRequests count 1000
  print "Calculating the MST"
  tree <- mst count weights :: IO (IOArray Int (Maybe Int))
  runM
    $ runTraceIO
    $ runOutputAsTrace @(Int, Int)
    $ runListInput requests
    $ runArvyLocal @IO @IOArray count weights tree ivy

  return ()
