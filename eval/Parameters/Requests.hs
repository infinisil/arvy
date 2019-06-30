module Parameters.Requests where

import qualified Data.Tree as T
import Polysemy
import Data.Array.IArray
import Polysemy.RandomFu
import Arvy.Local
import Utils
import Data.Ord
import Data.List
import Data.Random.Distribution.Uniform

data RequestsParameter r = RequestsParameter
  { requestsName :: String
  , requestsGet  :: Int -> GraphWeights -> Array Int (Maybe Int) -> Sem r Int
  }

worst :: RequestsParameter r
worst = RequestsParameter
  { requestsName = "worst"
  , requestsGet = \_ weights tree -> return $ fst $ maximumBy (comparing snd) (assocs (lengthsToRoot weights tree))

  } where
  -- | Computes the distance from all nodes to the root in /O(n)/
  lengthsToRoot :: (Num n, IArray arr n) => arr (Int, Int) n -> Array Int (Maybe Int) -> Array Int n
  lengthsToRoot weights tree = loeb fs
    where
      fs = aimap (\i v -> \others -> maybe 0 (\o -> others ! o + weights ! (i, o)) v)
                  tree

random :: Member RandomFu r => RequestsParameter r
random = RequestsParameter
  { requestsName = "random"
  , requestsGet = get
  } where
  get :: Member RandomFu r => Int -> GraphWeights -> Array Int (Maybe Int) -> Sem r Int
  get n _ _ = sampleRVar (integralUniform 0 (n - 1))

interactive :: Member (Lift IO) r => RequestsParameter r
interactive = RequestsParameter
  { requestsName = "interactive"
  , requestsGet = \_ _ tree -> do
      -- TODO: Use haskeline, add haskeline effect to polysemy
      sendM $ putStrLn $ T.drawTree $ fmap show $ treeStructure tree
      read <$> sendM getLine
  }



