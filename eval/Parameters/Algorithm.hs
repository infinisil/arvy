module Parameters.Algorithm where

import           Arvy.Algorithm
import qualified Arvy.Algorithm.Collection as Arvy
import qualified Parameters.Tree           as Tree

data AlgorithmParameter r = forall s . AlgorithmParameter
  { algorithmName        :: String
  , algorithmInitialTree :: Tree.InitialTreeParameter s r
  , algorithmGet         :: Arvy s r
  }

arrow :: AlgorithmParameter r
arrow = AlgorithmParameter
  { algorithmName = "Arrow"
  , algorithmInitialTree = Tree.mst
  , algorithmGet = Arvy.arrow
  }

ivy :: Show s => Tree.InitialTreeParameter s r -> AlgorithmParameter r
ivy tree = AlgorithmParameter
  { algorithmName = "Ivy"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.ivy
  }

constantRing :: AlgorithmParameter r
constantRing = AlgorithmParameter
  { algorithmName = "Constant Ring"
  , algorithmInitialTree = Tree.semiCircles
  , algorithmGet = Arvy.constantRing
  }

half :: Show s => Tree.InitialTreeParameter s r -> AlgorithmParameter r
half tree = AlgorithmParameter
  { algorithmName = "half node"
  , algorithmInitialTree = tree
  , algorithmGet = Arvy.half
  }
