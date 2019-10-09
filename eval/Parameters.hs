{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments #-}

module Parameters where

import Arvy.Algorithm
import Arvy.Local
import Parameters.Requests
import Parameters.Algorithm
import Parameters.Weights
import Parameters.Tree
import Arvy.Log
import qualified Data.Sequence as S
import Data.NonNull hiding (last)

import Polysemy
import GHC.Word
import Polysemy.Trace
import Conduit
import Evaluation.Types
import qualified Data.Conduit.Combinators as C
import Arvy.Weight
import Data.List
import Data.Array.Unboxed
import Polysemy.RandomFu
import Utils

data SharedParams r = SharedParams
  { sharedParamRandomSeed :: Word32
  , sharedParamRequestCount :: Int
  , sharedParamRequests :: RequestsParameter r
  , sharedParamEvals :: [Eval r]
  }

data GenParams r = GenParams
  { genParamShared :: SharedParams r
  , genParamNodeCount :: NodeCount
  , genParamWeights :: WeightsParam r
  , genParamAlgs :: [(GenAlgParam StandardNodeData r, TreeParam r)]
  }

--instance Show (GenParams r) where
--  show GenParams { genParamShared = SharedParams { .. }, genParamGraph = GraphParam { .. }, .. } =
--    graphName

data StandardNodeData = StandardNodeData
  { standardSucc :: Node
  , standardWeights :: Node -> Weight
  }

instance HasState StandardNodeData ()

instance HasSuccessor StandardNodeData where
  getSuccessor = standardSucc

instance HasWeights StandardNodeData where
  getWeights = standardWeights

--graphToData :: Member (Lift IO) r => Word32 -> GenParams (RandomFu ': r) -> Sem r (ArvyData StandardNodeData)
--graphToData seed GenParams { .. } = do
--  weights <- runRandomSeed seed $ weightsGen graphWeights graphNodeCount
--  tree <- runRandomSeed seed $ treeGen graphTree graphNodeCount weights

evalsConduit
  :: ( Traversable f
     , Member Trace r )
  => String
  -> f (Eval r)
  -> Env
  -> ConduitT (NonNull (S.Seq Node)) Void (Sem r) (f Series)
evalsConduit prefix evals env = --C.iterM (trace . show) .|
  sequenceConduits (fmap (\eval -> do
                             vals <- evalFun eval env .| C.sinkList
                             lift $ trace $ prefix ++ ", " ++ evalName eval ++ ": " ++ show (last vals)
                             return vals
                         ) evals)

getWeightsArray :: HasWeights a => ArvyData a -> GraphWeights
getWeightsArray ArvyData { .. } = listArray ((0, 0), (arvyDataNodeCount - 1, arvyDataNodeCount - 1)) weights where
  weights = [ getWeights (arvyDataNodeData u) v
            | u <- [0..arvyDataNodeCount - 1]
            , v <- [0..arvyDataNodeCount - 1]
            ]

runGenParams
  :: forall r
   . ( LogMember r
     , Members '[Lift IO, Trace] r )
  => GenParams (RandomFu ': r)
  -> Sem r EvalResults
runGenParams GenParams { genParamShared = SharedParams { .. }, .. } = do
  weights <- runRand $ weightsGen genParamWeights genParamNodeCount
  series <- transpose <$> mapM (runAlg weights) genParamAlgs
  return $ EvalResults "TODO" $ zip
    (map evalName sharedParamEvals)
    (map (zip (map (\(alg, tree) -> genAlgName alg ++ "-" ++ treeName tree) genParamAlgs)) series)
  where
    runAlg :: GraphWeights -> (GenAlgParam StandardNodeData (RandomFu ': r), TreeParam (RandomFu ': r)) -> Sem r [Series]
    runAlg weights (GenAlgParam name (GeneralArvy spec), TreeParam { .. }) = do
      tree <- runRand $ treeGen genParamNodeCount weights
      let arvyData = ArvyData
            { arvyDataNodeCount = genParamNodeCount
            , arvyDataNodeData = \node -> StandardNodeData
              { standardSucc = tree ! node
              , standardWeights = \other -> weights ! (node, other)
              }
            }

      (mutableTree, conduit) <- runRand $ runArvySpecLocal' @(S.Seq Int) arvyData spec
      let env = Env
            { envNodeCount = arvyDataNodeCount arvyData
            , envRequestCount = sharedParamRequestCount
            , envWeights = getWeightsArray arvyData
            , envTree = mutableTree
            }
          evals = evalsConduit name sharedParamEvals env
      request <- runRand $ requestsGet sharedParamRequests env
      runRand $ runConduit $ C.replicateM sharedParamRequestCount request
        .| C.mapM (\node -> (node `ncons`) <$> conduit node) .| evals
    runRand :: Sem (RandomFu ': r) x -> Sem r x
    runRand = runRandomSeed sharedParamRandomSeed





data SpecParams r = forall p a . SpecParams
  { specParamShared :: SharedParams r
  , specParamInit :: p
  , specParamAlg :: SpecAlgParam p a r
  , specParamGenAlgs :: [GenAlgParam a r]
  }
