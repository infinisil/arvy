{-# LANGUAGE BlockArguments      #-}
module Arvy.Requests where

import           Arvy.Utils
import           Arvy.Weights
import           Data.Array.IArray
import           Data.Array.MArray
import           Data.Foldable     (maximumBy)
import           Data.Ord          (comparing)
import           Polysemy
import           Polysemy.Input
import           Polysemy.Random
import           Polysemy.State

data RequestsParameter = RequestsParameter
  { requestsName :: String
  , requestsGet  :: forall r . Member Random r => Int -> GraphWeights -> Array Int (Maybe Int) -> Sem r Int
  }

runRequests
  :: forall m r arr a
  . ( Member (Lift m) r
    , MArray arr (Maybe Int) m
    , Member Random r )
  => Int
  -> GraphWeights
  -> arr Int (Maybe Int)
  -> RequestsParameter
  -> Int
  -> Sem (Input (Maybe Int) ': r) a
  -> Sem r a
runRequests nodeCount weights tree RequestsParameter { requestsGet } requestCount =
  fmap snd . runState requestCount . reinterpret \case
    Input -> get @Int >>= \case
      0 -> return Nothing
      k -> do
        put (k - 1)
        immutableTree <- sendM @m (freeze tree)
        request <- raise $ requestsGet nodeCount weights immutableTree
        return $ Just request

randomRequests :: RequestsParameter
randomRequests = RequestsParameter
  { requestsName = "random"
  , requestsGet = \n _ _ -> randomR (0, n - 1)
  }

worstRequests :: RequestsParameter
worstRequests = RequestsParameter
  { requestsName = "worst"
  , requestsGet = \_ weights tree -> return $ worstRequest weights tree
  }

-- | Computes the worst possible node to make a request by choosing the node that has the longest path to the root
worstRequest :: GraphWeights -> Array Int (Maybe Int) -> Int
worstRequest weights tree = fst $ maximumBy (comparing snd) (assocs (lengthsToRoot weights tree))

-- | Computes the distance from all nodes to the root in /O(n)/
lengthsToRoot :: (Num n, IArray arr n) => arr (Int, Int) n -> Array Int (Maybe Int) -> Array Int n
lengthsToRoot weights tree = loeb fs
  where
    fs = amap' (\i v -> \others -> maybe 0 (\o -> others ! o + weights ! (i, o)) v)
                tree

