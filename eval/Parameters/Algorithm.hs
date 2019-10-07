module Parameters.Algorithm where

import           Arvy.Algorithm
import qualified Arvy.Algorithm.Collection as Arvy
import           Data.Ratio
import Arvy.Weight

data GenAlgParam a r = GenAlgParam
  { genAlgName :: String
  , genAlg :: GeneralArvy a r
  }

data SpecAlgParam p a r = SpecAlgParam
  { specAlgName :: String
  , specAlg :: SpecializedArvy p a r
  }

arrow :: HasState a () => GenAlgParam a r
arrow = GenAlgParam "arrow" Arvy.arrow

minWeight :: (HasWeights a, HasState a ()) => GenAlgParam a r
minWeight = GenAlgParam "minWeight" Arvy.minWeight

ivy :: HasState a () => GenAlgParam a r
ivy = GenAlgParam "ivy" Arvy.ivy


ring :: SpecAlgParam NodeCount Arvy.RingArvyData r
ring = SpecAlgParam "ring" Arvy.ring

inbetween :: HasState a () => Ratio Int -> GenAlgParam a r
inbetween ratio = GenAlgParam ("inbetween-" ++ show ratio) (Arvy.inbetween ratio)

{-

inbetweenWeighted :: Show s => Double -> Tree.InitialTreeParameter s r -> AlgorithmParameter r
inbetweenWeighted ratio tree = AlgorithmParameter
  { algorithmId = "inbetweenWeighted" ++ show ratio ++ "-" ++ Tree.initialTreeId tree
  , algorithmDescription = "Inbetween, weighted"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.inbetweenWeighted ratio
  }

random :: (Show s, Member RandomFu r) => Tree.InitialTreeParameter s r -> AlgorithmParameter r
random tree = AlgorithmParameter
  { algorithmId = "random-" ++ Tree.initialTreeId tree
  , algorithmDescription = "Random"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.random
  }

utilityFun :: (Show s, Ord a) => String -> (Int -> Double -> a) -> Tree.InitialTreeParameter s r -> AlgorithmParameter r
utilityFun desc f tree = AlgorithmParameter
  { algorithmId = "u-" ++ desc
  , algorithmDescription = "Utility function " ++ desc
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.utilityFun f
  }

indexMeanScore :: Member Trace r => Arvy.IndexMeanType -> (Int -> Double) -> Tree.InitialTreeParameter (Arvy.IndexMean, Int) r -> AlgorithmParameter r
indexMeanScore ty af tree = AlgorithmParameter
  { algorithmId = "indexmean-wip"
  , algorithmDescription = "Index mean weighted"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.indexMeanScore ty af
  }


localMinPairs :: (Member Trace r, Show s) => Tree.InitialTreeParameter s r -> AlgorithmParameter r
localMinPairs tree = AlgorithmParameter
  { algorithmId = "localminpairs-" ++ Tree.initialTreeId tree
  , algorithmDescription = "Local minimum pair distances"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.localMinPairs
  }
-}
