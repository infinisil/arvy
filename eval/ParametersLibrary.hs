module ParametersLibrary where

import           Arvy.Requests
import           Arvy.Weights
import Arvy.Tree
import           Parameters
import Polysemy
import Polysemy.Random
import Data.Array


pRandomWeights :: WeightsParameter
pRandomWeights = WeightsParameter
  { weightsName = "random"
  , weightsGet = randomWeights
  }

pRandomRequests :: RequestsParameter
pRandomRequests = RequestsParameter
  { requestsName = "random"
  , requestsGet = get
  } where
  get :: Member Random r => Int -> GraphWeights -> Array Int (Maybe Int) -> Sem r Int
  get n _ _ = randomRequest n
  
pRingWeights :: WeightsParameter
pRingWeights = WeightsParameter "ring" (return . ringWeights)

pMst :: InitialTreeParameter
pMst = InitialTreeParameter "mst" (\n w -> return (mst n w))
