module Parameters.Requests where

import qualified Data.Tree as T
import Polysemy
import Data.Array.Unboxed
import Polysemy.RandomFu
import Arvy.Local
import Utils
import Data.Ord
import Data.List
import Data.Random.Distribution.Uniform

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

interactive :: Member (Lift IO) r => RequestsParameter r
interactive = RequestsParameter
  { requestsName = "interactive"
  , requestsGet = \_ _ tree -> do
      -- TODO: Use haskeline, add haskeline effect to polysemy
      sendM $ putStrLn $ T.drawTree $ fmap show $ treeStructure tree
      read <$> sendM getLine
  }



