{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           AlgorithmSpec
import           CollectionSpec
import           Control.DeepSeq
import           Control.Exception
import           Data.Array.Unboxed
import qualified Data.Tree          as T
import           Evaluation.Tree
import qualified Parameters.Tree    as Tree
import qualified Parameters.Weights as Weights
import           Polysemy
import           Test.Hspec
import           Utils

main :: IO ()
main = hspec $ do
  algorithmSpec
  collectionSpec

  describe "Parameters.Weights.shortestPathWeights" $ do
    it "calculates the transitive shortest paths" $
      Weights.shortestPathWeights 3 (0 * 1 + 1 * 2) ! (0, 2) `shouldBe` 2

    it "returns infinity for paths that don't exist" $
      Weights.shortestPathWeights 2 (0 + 1) ! (0, 1) `shouldBe` infinity

    it "finds the shorter path of multiple" $
      Weights.shortestPathWeights 5 (0 * 1 + 1 * 2 + 2 * 3 + 0 * 4 + 4 * 3) ! (0, 3)
        `shouldBe` 2

    it "throws an error for vertices holes" $
      evaluate (Weights.shortestPathWeights 2 (0 * 2)) `shouldThrow` anyErrorCall

    it "assigns 0 to paths from nodes to themselves" $
      Weights.shortestPathWeights 1 0 ! (0, 0) `shouldBe` 0

    it "correctly assigns weights to a 4-node ring" $
      run (Weights.weightsGet Weights.ring 4) `shouldBe` listArray ((0, 0), (3, 3))
        [ 0, 1, 2, 1
        , 1, 0, 1, 2
        , 2, 1, 0, 1
        , 1, 2, 1, 0
        ]

  describe "Utils.treeStructure" $ do
    it "finds the structure with a single node" $
      treeStructure (listArray (0, 0) [0]) `shouldBe` T.Node 0 []

    it "finds the structure with every node pointing to the root" $
      treeStructure (listArray (0, 3) (repeat 0)) `shouldBe` T.Node 0 [T.Node 1 [], T.Node 2 [], T.Node 3 []]

    it "finds the structure with indirect pointers" $
      treeStructure (listArray (0, 3) (0 : [0..])) `shouldBe` T.Node 0 [T.Node 1 [T.Node 2 [T.Node 3 []]]]

    it "finds the structure of a binary tree" $
      treeStructure (listArray (0, 6) [0, 0, 1, 1, 0, 4, 4])
        `shouldBe` T.Node 0 [T.Node 1 [T.Node 2 [], T.Node 3 []], T.Node 4 [T.Node 5 [], T.Node 6 []]]

    it "errors when there's no root" $
      evaluate (force (treeStructure (listArray (0, 3) [1, 0, 0, 2])))
        `shouldThrow` errorCall "Tree has no root"

    it "errors when there's multiple roots" $
      evaluate (force (treeStructure (listArray (0, 3) [1, 1, 2, 1])))
        `shouldThrow` errorCall "Tree has multiple roots at both node 1 and 2"

    it "doesn't error on lost nodes" $
      treeStructure (listArray (0, 3) [0, 0, 3, 2]) `shouldBe` T.Node 0 [T.Node 1 []]


  describe "Evaluation.Tree.avgTreeStretchDiameter" $ do
    it "works on a 3-node ring tree and graph" $ do
      let (stretch, diam) = avgTreeStretchDiameter 3 (run $ Weights.weightsGet Weights.ring 3) (fst $ run $ Tree.initialTreeGet Tree.ring 3 undefined)
      stretch `shouldBeAbout` (1 + 1 / 3)
      diam `shouldBe` 2

    it "works on a 5-node ring tree and graph" $ do
      let (stretch, diam) = avgTreeStretchDiameter 5 (run $ Weights.weightsGet Weights.ring 5) (fst $ run $ Tree.initialTreeGet Tree.ring 5 undefined)
      stretch `shouldBeAbout` 1.4
      diam `shouldBe` 4

shouldSatisfyReturn :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
shouldSatisfyReturn action expected = action >>= (`shouldSatisfy` expected)

shouldBeAbout :: (HasCallStack, Show a, Ord a, Fractional a) => a -> a -> Expectation
shouldBeAbout v e = v `shouldSatisfy` (< 0.000001) . abs . subtract e
