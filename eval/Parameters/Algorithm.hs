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

edgeMin :: GenAlgParam r
edgeMin = GenAlgParam "edgeMin" Arvy.edgeMin

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

fixedRatio :: Ratio Int -> GenAlgParam r
fixedRatio ratio = GenAlgParam ("fixedRatio-" <> tshow (numerator ratio) <> "-" <> tshow (denominator ratio)) (Arvy.fixedRatio ratio)

weightedFixedRatio :: Double -> GenAlgParam r
weightedFixedRatio ratio = GenAlgParam ("weightedFixedRatio-" <> tshow ratio) (Arvy.weightedFixedRatio ratio)

dynamicStar :: (Member (Lift IO) r, LogMember r) => GenAlgParam r
dynamicStar = GenAlgParam "dynamicStar" Arvy.dynamicStar

indexMeanHop :: LogMember r => GenAlgParam r
indexMeanHop = GenAlgParam "indexMeanHop" (Arvy.indexMeanScore Arvy.HopIndexBased (const 0.2))

indexMeanWeight :: LogMember r => GenAlgParam r
indexMeanWeight = GenAlgParam "indexMeanWeight" (Arvy.indexMeanScore Arvy.WeightSumBased (const 0.2))
