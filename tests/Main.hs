{-# LANGUAGE DataKinds #-}

module Main where

import           Arvy.Utils
import           Arvy.Weights
import           Control.Exception
import           Data.Array.Unboxed
import           Polysemy
import           Polysemy.Random
import           System.Random      (mkStdGen)
import           Test.Hspec

withSeed :: Int -> Sem '[Random] a -> a
withSeed seed = snd . run . runRandom (mkStdGen seed)

main :: IO ()
main = hspec $
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
