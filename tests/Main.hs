{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import           Arvy.Tree
import           Arvy.Utils
import           Arvy.Weights
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.IO
import           Data.Array.ST
import           Data.Array.Unboxed
import qualified Data.Tree          as T
import           Polysemy
import           Polysemy.Output
import           Polysemy.Random
import           Polysemy.Trace
import           System.Random      (mkStdGen)
import           Test.Hspec

withSeed :: Int -> Sem '[Random] a -> a
withSeed seed = snd . run . runRandom (mkStdGen seed)

main :: IO ()
main = hspec $ do
  describe "Arvy.Weights.shortestPathWeights" $ do
    it "calculates the transitive shortest paths" $
      shortestPathWeights (0 * 1 + 1 * 2) ! (0, 2) `shouldBe` 2

    it "returns infinity for paths that don't exist" $
      shortestPathWeights (0 + 1) ! (0, 1) `shouldBe` infinity

    it "finds the shorter path of multiple" $
      shortestPathWeights (0 * 1 + 1 * 2 + 2 * 3 + 0 * 4 + 4 * 3) ! (0, 3)
        `shouldBe` 2

    it "throws an error for vertices holes" $
      evaluate (shortestPathWeights (0 * 2)) `shouldThrow` anyErrorCall

    it "assigns 0 to paths from nodes to themselves" $
      shortestPathWeights 0 ! (0, 0) `shouldBe` 0

    it "correctly assigns weights to a 4-node ring" $
      ringWeights 4 `shouldBe` listArray ((0, 0), (3, 3))
        [ 0, 1, 2, 1
        , 1, 0, 1, 2
        , 2, 1, 0, 1
        , 1, 2, 1, 0
        ]

  describe "Arvy.Tree.treeStructure" $ do
    it "finds the structure with a single node" $
      treeStructure (listArray (0, 0) [Nothing]) `shouldBe` T.Node 0 []

    it "finds the structure with every node pointing to the root" $
      treeStructure (listArray (0, 3) (Nothing : repeat (Just 0))) `shouldBe` T.Node 0 [T.Node 1 [], T.Node 2 [], T.Node 3 []]

    it "finds the structure with indirect pointers" $
      treeStructure (listArray (0, 3) (Nothing : map Just [0..])) `shouldBe` T.Node 0 [T.Node 1 [T.Node 2 [T.Node 3 []]]]

    it "finds the structure of a binary tree" $
      treeStructure (listArray (0, 6) [Nothing, Just 0, Just 1, Just 1, Just 0, Just 4, Just 4])
        `shouldBe` T.Node 0 [T.Node 1 [T.Node 2 [], T.Node 3 []], T.Node 4 [T.Node 5 [], T.Node 6 []]]

    it "errors when there's no root" $
      evaluate (force (treeStructure (listArray (0, 3) [Just 1, Just 0, Just 2, Just 1])))
        `shouldThrow` errorCall "Tree has no root"

    it "errors when there's multiple roots" $
      evaluate (force (treeStructure (listArray (0, 3) [Just 1, Nothing, Nothing, Just 1])))
        `shouldThrow` errorCall "Tree has multiple roots at both node 1 and 2"

    it "doesn't error on lost nodes" $
      treeStructure (listArray (0, 3) [Nothing, Just 0, Just 3, Just 2]) `shouldBe` T.Node 0 [T.Node 1 []]


  describe "Arvy.Tree.avgTreeStretch" $ do
    it "works on a 3-node ring tree and graph" $
      avgTreeStretch 3 (ringWeights 3) (ringTree 3) `shouldBeAbout` (1 + 1 / 3)

    it "works on a 5-node ring tree and graph" $
      avgTreeStretch 5 (ringWeights 5) (ringTree 5) `shouldBeAbout` 1.4

    it "is greater or equal to 1 for 100 random samples" $
      runM (fmap snd $ runRandom (mkStdGen 0) $ replicateM 100 randomStretch) `shouldSatisfyReturn` all (>= 1)

randomStretch :: Member Random r => Sem r Double
randomStretch = do
  n <- randomR (1, 100)
  points <- randomPoints n
  let weights = euclidianWeights points
  let tree = mst n weights
  return $ avgTreeStretch n weights tree

shouldSatisfyReturn :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
shouldSatisfyReturn action expected = action >>= (`shouldSatisfy` expected)

shouldBeAbout :: (HasCallStack, Show a, Ord a, Fractional a) => a -> a -> Expectation
shouldBeAbout v e = v `shouldSatisfy` (< 0.000001) . abs . subtract e

