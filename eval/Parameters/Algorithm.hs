module Parameters.Algorithm where

import           Arvy.Algorithm
import qualified Arvy.Algorithm.Collection as Arvy
import qualified Parameters.Tree           as Tree

data AlgorithmParameter r = forall s . AlgorithmParameter
  { algorithmId        :: String
  , algorithmDescription :: String
  , algorithmInitialTree :: Tree.InitialTreeParameter s r
  , algorithmGet         :: Arvy s r
  }

arrow :: AlgorithmParameter r
arrow = AlgorithmParameter
  { algorithmId = "arrow"
  , algorithmDescription = "Arrow"
  , algorithmInitialTree = Tree.mst
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
