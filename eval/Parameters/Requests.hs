{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parameters.Requests
  ( RequestsParameter(..)
  , farthest
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
import Evaluation.Types
import Data.Array.MArray

data RequestsParameter r = RequestsParameter
  { requestsId :: String
  , requestsDescription :: String
  , requestsGet  :: Env -> Sem r (Sem r Int)
  }

instance Eq (RequestsParameter r) where
  RequestsParameter { requestsId = id1 } == RequestsParameter { requestsId = id2 } = id1 == id2

instance Ord (RequestsParameter r) where
  RequestsParameter { requestsId = id1 } `compare` RequestsParameter { requestsId = id2 } = id1 `compare` id2

instance Show (RequestsParameter r) where
  show RequestsParameter { requestsDescription = desc } = desc

farthest :: Member (Lift IO) r => RequestsParameter r
farthest = RequestsParameter
  { requestsId = "farthest"
  , requestsDescription = "Farthest"
  , requestsGet = \Env { envWeights = weights, envTree = tree' } -> do
      tree <- sendM $ freeze tree'
      return $ return $ fst $ maximumBy (comparing snd) (assocs (lengthsToRoot weights tree))
  } where
  -- | Computes the distance from all nodes to the root in /O(n)/
  lengthsToRoot :: GraphWeights -> RootedTree -> Array Int Double
  lengthsToRoot weights tree = loeb fs
    where
      fs :: Array Int (Array Int Double -> Double)
      fs = aimap (\i o others -> if i == o then 0
                   else others ! o + weights ! (i, o))
                  tree

random :: Member RandomFu r => RequestsParameter r
random = RequestsParameter
  { requestsId = "random"
  , requestsDescription = "Uniformly random requests"
  , requestsGet = return . get
  } where
  get :: Member RandomFu r => Env -> Sem r Int
  get Env { envNodeCount = n } = sampleRVar (integralUniform 0 (n - 1))


lorenz :: Distribution Lorenz a => a -> RVar a
lorenz a = rvar (Lorenz a)


newtype Lorenz a = Lorenz a

-- https://en.wikipedia.org/wiki/Pareto_distribution#Lorenz_curve_and_Gini_coefficient
instance (Floating a, Distribution StdUniform a) => Distribution Lorenz a where
  rvarT (Lorenz a) = do
    u <- stdUniformT
    return $ 1 - (1 - u) ** (1 - recip a)

pareto :: forall r . Member RandomFu r => RequestsParameter r
pareto = RequestsParameter
  { requestsId = "pareto"
  , requestsDescription = "80-20 Pareto distribution (alpha = log 5 / log 4)"
  , requestsGet = \Env { envNodeCount = n } -> do
      -- Generate a random node order such that in graphs whose node indices indicate the graph structure this distribution is truly random
      order <- listArray @UArray (0, n - 1) <$> sampleRVar (shuffle [0 .. n - 1])
      return $ do
        v <- sampleRVar (lorenz a)
        return $ order ! floor (v * fromIntegral n)
  } where
  -- alpha for lorenz curve for which 80% of requests go to 20% of nodes
  a :: Double
  a = logBase 4 5

interactive :: Member (Lift IO) r => RequestsParameter r
interactive = RequestsParameter
  { requestsId = "interactive"
  , requestsDescription = "Interactive"
  , requestsGet = \Env { envTree = tree' } -> return $ do
      tree <- sendM $ freeze tree'
      -- TODO: Use haskeline, add haskeline effect to polysemy
      sendM $ putStrLn $ T.drawTree $ show <$> treeStructure tree
      read <$> sendM getLine
  }
