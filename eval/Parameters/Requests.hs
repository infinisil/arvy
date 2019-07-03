{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parameters.Requests
  ( RequestsParameter(..)
  , worst
  , random
  , pareto
  , interactive
  ) where

import qualified Data.Tree as T
import Polysemy
import Data.Array.Unboxed
import Polysemy.RandomFu
import Arvy.Local
import Utils
import Data.Ord
import Data.List
import Data.Random.Distribution.Uniform
import Data.Random

data RequestsParameter r = RequestsParameter
  { requestsName :: String
  , requestsGet  :: Int -> GraphWeights -> RootedTree -> Sem r Int
  }

worst :: RequestsParameter r
worst = RequestsParameter
  { requestsName = "worst"
  , requestsGet = \_ weights tree -> return $ fst $ maximumBy (comparing snd) (assocs (lengthsToRoot weights tree))

  } where
  -- | Computes the distance from all nodes to the root in /O(n)/
  lengthsToRoot :: GraphWeights -> RootedTree -> Array Int Double
  lengthsToRoot weights tree = loeb fs
    where
      fs :: Array Int (Array Int Double -> Double)
      fs = aimap (\i v -> \others -> (\o -> others ! o + weights ! (i, o)) v)
                  tree

random :: Member RandomFu r => RequestsParameter r
random = RequestsParameter
  { requestsName = "random"
  , requestsGet = get
  } where
  get :: Member RandomFu r => Int -> GraphWeights -> RootedTree -> Sem r Int
  get n _ _ = sampleRVar (integralUniform 0 (n - 1))


lorenz :: Distribution Lorenz a => a -> RVar a
lorenz a = rvar (Lorenz a)


newtype Lorenz a = Lorenz a

-- https://en.wikipedia.org/wiki/Pareto_distribution#Lorenz_curve_and_Gini_coefficient
instance (Floating a, Distribution StdUniform a) => Distribution Lorenz a where
  rvarT (Lorenz a) = do
    u <- stdUniformT
    return $ 1 - (1 - u) ** (1 - recip a)

-- TODO: Use random mapping of nodes instead of always choosing the lower 20% the most
pareto :: Member RandomFu r => RequestsParameter r
pareto = RequestsParameter
  { requestsName = "80-20 Pareto distribution (alpha = log 5 / log 4)"
  , requestsGet = \n _ _ -> do
      v <- sampleRVar (lorenz a)
      return $ floor (v * fromIntegral n)
  } where
  a :: Double
  a = logBase 4 5

interactive :: Member (Lift IO) r => RequestsParameter r
interactive = RequestsParameter
  { requestsName = "interactive"
  , requestsGet = \_ _ tree -> do
      -- TODO: Use haskeline, add haskeline effect to polysemy
      sendM $ putStrLn $ T.drawTree $ fmap show $ treeStructure tree
      read <$> sendM getLine
  }



