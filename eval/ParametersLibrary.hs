module ParametersLibrary where

import           Arvy.Requests
import           Arvy.Weights
import Arvy.Tree
import           Parameters
import Polysemy
import Polysemy.Random
import Data.Array

pWorstRequests :: RequestsParameter r
pWorstRequests = RequestsParameter
  { requestsName = "worst"
  , requestsGet = get
  } where
  get :: Int -> GraphWeights -> Array Int (Maybe Int) -> Sem r Int
  get _ weights tree = return $ worstRequest weights tree


pRandomRequests :: Member Random r => RequestsParameter r
pRandomRequests = RequestsParameter
  { requestsName = "random"
  , requestsGet = get
  } where
  get :: Member Random r => Int -> GraphWeights -> Array Int (Maybe Int) -> Sem r Int
  get n _ _ = randomRequest n

pInteractiveRequests :: Member (Lift IO) r => RequestsParameter r
pInteractiveRequests = RequestsParameter
  { requestsName = "interactive"
  , requestsGet = \_ _ -> interactiveRequests
  }

pRingWeights :: WeightsParameter r
pRingWeights = WeightsParameter "ring" (return . ringWeights)

pMst :: InitialTreeParameter r
pMst = InitialTreeParameter "mst" (\n w -> return (mst n w))

pRing :: InitialTreeParameter r
pRing = InitialTreeParameter "ring" (\n _ -> return (ringTree n))

pSemiCircles :: InitialTreeParameter r
pSemiCircles = InitialTreeParameter "semi circles" (\n _ -> return (semiCircles n))

pBarabasiWeights :: Member (Lift IO) r => Int -> WeightsParameter r
pBarabasiWeights m = WeightsParameter
  { weightsName = "barabasi albert"
  , weightsGet = (`barabasiAlbert` m)
  }

