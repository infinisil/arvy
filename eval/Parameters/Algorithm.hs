module Parameters.Algorithm where

import           Arvy.Algorithm
import qualified Arvy.Algorithm.Collection as Arvy
import qualified Parameters.Tree           as Tree
import Data.Ratio
import Polysemy
import Polysemy.RandomFu
import Polysemy.Trace

data AlgorithmParameter r = forall s . AlgorithmParameter
  { algorithmId        :: String
  , algorithmDescription :: String
  , algorithmInitialTree :: Tree.InitialTreeParameter s r
  , algorithmGet         :: Arvy s r
  }

instance Eq (AlgorithmParameter r) where
  AlgorithmParameter { algorithmId = id1 } == AlgorithmParameter { algorithmId = id2 } = id1 == id2

instance Ord (AlgorithmParameter r) where
  AlgorithmParameter { algorithmId = id1 } `compare` AlgorithmParameter { algorithmId = id2 } = id1 `compare` id2

instance Show (AlgorithmParameter r) where
  show AlgorithmParameter { algorithmDescription = desc } = desc

genArrow :: Show s => Tree.InitialTreeParameter s r -> AlgorithmParameter r
genArrow tree = AlgorithmParameter
  { algorithmId = "genArrow-" ++ Tree.initialTreeId tree
  , algorithmDescription = "Generalized Arrow, always choosing the node with lowest weight"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.genArrow
  }

arrow :: Show s => Tree.InitialTreeParameter s r -> AlgorithmParameter r
arrow tree = AlgorithmParameter
  { algorithmId = "arrow-" ++ Tree.initialTreeId tree
  , algorithmDescription = "Arrow"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.arrow
  }

ivy :: Show s => Tree.InitialTreeParameter s r -> AlgorithmParameter r
ivy tree = AlgorithmParameter
  { algorithmId = "ivy-" ++ Tree.initialTreeId tree
  , algorithmDescription = "Ivy"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.ivy
  }

constantRing :: AlgorithmParameter r
constantRing = AlgorithmParameter
  { algorithmId = "ring"
  , algorithmDescription = "Constant ring"
  , algorithmInitialTree = Tree.semiCircles
  , algorithmGet = Arvy.constantRing
  }

half :: Show s => Tree.InitialTreeParameter s r -> AlgorithmParameter r
half tree = AlgorithmParameter
  { algorithmId = "half-" ++ Tree.initialTreeId tree
  , algorithmDescription = "Half node"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.half
  }

inbetween :: Show s => Ratio Int -> Tree.InitialTreeParameter s r -> AlgorithmParameter r
inbetween ratio tree = AlgorithmParameter
  { algorithmId = "inbetween" ++ show (numerator ratio) ++ "%" ++ show (denominator ratio) ++ "-" ++ Tree.initialTreeId tree
  , algorithmDescription = "Inbetween"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.inbetween ratio
  }

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
