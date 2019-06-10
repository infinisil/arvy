{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Arvy.Requests where

import Polysemy
import Arvy.Tree
import Arvy.Weights
import Polysemy.Input
import Data.Array.IArray
import Polysemy.State
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Arvy.Utils
import Polysemy.Random


type Requests r = Members '[] r => Int -> GraphWeights -> (forall a . Sem (Input (Maybe Int) ': r) a -> Sem r a)

requestsFromList :: [Int] -> Requests r
requestsFromList list _ _ = runListInput list

randomRequests :: Member Random r => Int -> Int -> Sem r [Int]
randomRequests _ 0 = return []
randomRequests n k = do
  r <- randomR (0, n - 1)
  (r:) <$> randomRequests n (k - 1)

-- | TODO: Shouldn't take tree input like this. Pass it as an argument in Requests directly
requestsWorst :: Int -> Array Int (Maybe Int) -> Requests r
requestsWorst count tree n weights = fmap snd . runState count . reinterpret \case
  Input -> do
    get @Int >>= \case
      0 -> return Nothing
      _ -> do
        modify @Int (subtract 1)
        let (x, _) = maximumBy (comparing snd) (assocs (lengthsToRoot weights tree))
        return (Just x)

-- | Computes the distance from all nodes to the root in O(n)
lengthsToRoot :: GraphWeights -> Array Int (Maybe Int) -> Array Int Double
lengthsToRoot weights tree = loeb fs
  where
    fs = amap' (\i v -> \others -> maybe 0 (\o -> others ! o + weights ! (i, o)) v)
                tree

