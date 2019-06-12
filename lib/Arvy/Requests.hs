{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
module Arvy.Requests where

import Polysemy
import Arvy.Weights
import Polysemy.Input
import Data.Array.IArray
import Polysemy.State
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Arvy.Utils
import Polysemy.Random
import Data.Array.MArray

type Requests r = forall a . Sem (Input (Maybe Int) ': r) a -> Sem r a

requestsFromList :: [Int] -> Requests r
requestsFromList = runListInput

randomRequests :: Member Random r => Int -> Int -> Requests r
randomRequests n count = fmap snd . runState count . reinterpret \case
  Input -> get @Int >>= \case
    0 -> return Nothing
    _ -> do
      modify @Int (subtract 1)
      r <- randomR (0, n - 1)
      return $ Just r
      
requestsWorst :: forall m arr r . (MArray arr (Maybe Int) m, Member (Lift m) r) => Int -> GraphWeights -> arr Int (Maybe Int) -> Requests r
requestsWorst count weights tree = fmap snd . runState count . reinterpret \case
  Input -> get @Int >>= \case
    0 -> return Nothing
    _ -> do
      modify @Int (subtract 1)
      -- TODO: Investigate whether unsafeFreeze is safe in this case, might get a big performance boost
      -- Which might be the case because in order for the algorithm to modify the tree, it needs to know what request has been issued which requires evaluating x.
      -- And only x depends on the immutable unsafe version of the tree. So after we calculated x, we will never use the immutable tree again, therefore any future mutations won't be a problem.
      tree' <- sendM @m (freeze tree)
      let r = worstRequest weights tree'
      return $ Just r

-- | Computes the worst possible node to make a request by choosing the node that has the longest path to the root
worstRequest :: GraphWeights -> Array Int (Maybe Int) -> Int
worstRequest weights tree = fst $ maximumBy (comparing snd) (assocs (lengthsToRoot weights tree))

-- | Computes the distance from all nodes to the root in O(n)
lengthsToRoot :: (Num n, IArray arr n) => arr (Int, Int) n -> Array Int (Maybe Int) -> Array Int n
lengthsToRoot weights tree = loeb fs
  where
    fs = amap' (\i v -> \others -> maybe 0 (\o -> others ! o + weights ! (i, o)) v)
                tree

