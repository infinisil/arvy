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


randomRequest :: Member RandomFu r => Int -> Sem r Int
randomRequest n = sampleRVar (integralUniform 0 (n - 1))

-- | Computes the worst possible node to make a request by choosing the node that has the longest path to the root
worstRequest :: GraphWeights -> Array Int (Maybe Int) -> Int
worstRequest weights tree = fst $ maximumBy (comparing snd) (assocs (lengthsToRoot weights tree))

-- | Computes the distance from all nodes to the root in /O(n)/
lengthsToRoot :: (Num n, IArray arr n) => arr (Int, Int) n -> Array Int (Maybe Int) -> Array Int n
lengthsToRoot weights tree = loeb fs
  where
    fs = aimap (\i v -> \others -> maybe 0 (\o -> others ! o + weights ! (i, o)) v)
                tree

-- TODO: Use haskeline, add haskeline effect to polysemy
interactiveRequests :: Member (Lift IO) r => Array Int (Maybe Int) -> Sem r Int
interactiveRequests tree = do
  sendM $ putStrLn $ T.drawTree $ fmap show $ treeStructure tree
  read <$> sendM getLine
