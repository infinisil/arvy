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
import qualified Data.Tree as T
import Arvy.Tree

runRequests
  :: forall m r arr a
  . ( Member (Lift m) r
    , MArray arr (Maybe Int) m )
  => arr Int (Maybe Int)
  -> (Array Int (Maybe Int) -> Sem r Int)
  -> Int
  -> Sem (Input (Maybe Int) ': r) a
  -> Sem r a
runRequests tree getRequest requestCount =
  fmap snd . runState requestCount . reinterpret \case
    Input -> get >>= \case
      0 -> return Nothing
      k -> do
        put (k - 1)
        immutableTree <- sendM $ freeze tree
        Just <$> raise (getRequest immutableTree)

randomRequest :: Member Random r => Int -> Sem r Int
randomRequest n = randomR (0, n - 1)

-- | Computes the worst possible node to make a request by choosing the node that has the longest path to the root
worstRequest :: GraphWeights -> Array Int (Maybe Int) -> Int
worstRequest weights tree = fst $ maximumBy (comparing snd) (assocs (lengthsToRoot weights tree))

-- | Computes the distance from all nodes to the root in /O(n)/
lengthsToRoot :: (Num n, IArray arr n) => arr (Int, Int) n -> Array Int (Maybe Int) -> Array Int n
lengthsToRoot weights tree = loeb fs
  where
    fs = amap' (\i v -> \others -> maybe 0 (\o -> others ! o + weights ! (i, o)) v)
                tree

-- TODO: Use haskeline, add haskeline effect to polysemy
interactiveRequests :: Member (Lift IO) r => Array Int (Maybe Int) -> Sem r Int
interactiveRequests tree = do
  sendM $ putStrLn $ T.drawTree $ fmap show $ treeStructure tree
  read <$> sendM getLine
