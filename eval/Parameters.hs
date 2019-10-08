{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments #-}

module Parameters where

import Arvy.Algorithm
import Arvy.Local
import Parameters.Requests
import Parameters.Algorithm
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

data SharedParams r = SharedParams
  { sharedParamRandomSeed :: Word32
  , sharedParamRequestCount :: Int
  , sharedParamRequests :: RequestsParameter r
  , sharedParamEvals :: [Eval r]
  }

data GenParams r = GenParams
  { genParamShared :: SharedParams r
  , genParamGraph :: GraphParam r
  , genParamAlgs :: [GenAlgParam StandardNodeData r]
  }

type WeightFun = Edge -> Weight
type Tree = Node -> Node

data GraphParam r = GraphParam
  { graphNodeCount :: NodeCount
  , graphWeights :: NodeCount -> Sem r WeightFun
  , graphTree :: NodeCount -> WeightFun -> Sem r Tree
  }

data StandardNodeData = StandardNodeData
  { standardSucc :: Node
  , standardWeights :: Node -> Weight
  }

instance HasSuccessor StandardNodeData where
  getSuccessor = standardSucc

instance HasWeights StandardNodeData where
  getWeights = standardWeights

graphToData :: GraphParam r -> Sem r (ArvyData StandardNodeData)
graphToData = undefined

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
getWeightsArray = undefined

runGenParams
  :: forall r
   . ( LogMember r
     , Members '[Lift IO, Trace] r )
  => GenParams r
  -> Sem r EvalResults
runGenParams GenParams { genParamShared = SharedParams { .. }, .. } = do
  dat <- graphToData genParamGraph
  series <- transpose <$> mapM (runAlg dat) genParamAlgs
  return $ EvalResults "TODO" $ zip
    (map evalName sharedParamEvals)
    (map (zip (map genAlgName genParamAlgs)) series)
  where
    runAlg :: ArvyData StandardNodeData -> GenAlgParam StandardNodeData r -> Sem r [Series]
    runAlg arvyData (GenAlgParam name (GeneralArvy spec)) = do
      (mutableTree, conduit) <- runArvySpecLocal' @(S.Seq Int) arvyData spec
      let env = Env
            { envNodeCount = arvyDataNodeCount arvyData
            , envRequestCount = sharedParamRequestCount
            , envWeights = getWeightsArray arvyData
            , envTree = mutableTree
            }
          evals = evalsConduit name sharedParamEvals env
      request <- requestsGet sharedParamRequests env
      runConduit $ C.replicateM sharedParamRequestCount request
        .| C.mapM (\node -> (node `ncons`) <$> conduit node) .| evals





data SpecParams r = forall p a . SpecParams
  { specParamShared :: SharedParams r
  , specParamInit :: p
  , specParamAlg :: SpecAlgParam p a r
  , specParamGenAlgs :: [GenAlgParam a r]
  }
