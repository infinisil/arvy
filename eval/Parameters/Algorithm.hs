{-# LANGUAGE OverloadedStrings #-}

module Parameters.Algorithm where

import           Arvy.Algorithm
import qualified Arvy.Algorithm.Collection as Arvy
import           Arvy.Log
import           Data.Ratio
import           Data.Text                 (Text)
import           Polysemy
import           Polysemy.RandomFu

data GenAlgParam r = GenAlgParam
  { genAlgName :: Text
  , genAlg     :: GeneralArvy r
  }

data SpecAlgParam p a r = SpecAlgParam
  { specAlgName :: Text
  , specAlg     :: SpecializedArvy p a r
  }

arrow :: GenAlgParam r
arrow = GenAlgParam "arrow" Arvy.arrow

minWeight :: GenAlgParam r
minWeight = GenAlgParam "minWeight" Arvy.minWeight

ivy :: GenAlgParam r
ivy = GenAlgParam "ivy" Arvy.ivy

random :: Member RandomFu r => GenAlgParam r
random = GenAlgParam "random" Arvy.random

localMinPairs :: LogMember r => GenAlgParam r
localMinPairs = GenAlgParam "localMinPairs" Arvy.localMinPairs

ring :: SpecAlgParam NodeCount Arvy.RingNodeState r
ring = SpecAlgParam "ring" Arvy.ring

reclique :: SpecAlgParam Arvy.RecliqueConf (Maybe Int) r
reclique = SpecAlgParam "reclique" Arvy.reclique

inbetween :: Ratio Int -> GenAlgParam r
inbetween ratio = GenAlgParam ("inbetween-" <> tshow (numerator ratio) <> "-" <> tshow (denominator ratio)) (Arvy.inbetween ratio)

inbetweenWeighted :: Double -> GenAlgParam r
inbetweenWeighted ratio = GenAlgParam ("inbetween2-" <> tshow ratio) (Arvy.inbetweenWeighted ratio)

dynamicStar :: (Member (Lift IO) r, LogMember r) => GenAlgParam r
dynamicStar = GenAlgParam "dynamicStar" Arvy.dynamicStar

indexMeanHop :: LogMember r => GenAlgParam r
indexMeanHop = GenAlgParam "indexMeanHop" (Arvy.indexMeanScore Arvy.HopIndexBased (const 0.2))

indexMeanWeight :: LogMember r => GenAlgParam r
indexMeanWeight = GenAlgParam "indexMeanWeight" (Arvy.indexMeanScore Arvy.WeightSumBased (const 0.2))

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
