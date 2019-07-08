module Parameters.Algorithm where

import           Arvy.Algorithm
import qualified Arvy.Algorithm.Collection as Arvy
import qualified Parameters.Tree           as Tree

data AlgorithmParameter r = AlgorithmParameter
  { algorithmName        :: String
  , algorithmInitialTree :: Tree.InitialTreeParameter r
  , algorithmGet         :: Int -> Arvy r
  }

arrow :: AlgorithmParameter r
arrow = AlgorithmParameter
  { algorithmName = "Arrow"
  , algorithmInitialTree = Tree.mst
  , algorithmGet = \_ -> Arvy.arrow
  }

ivy :: Tree.InitialTreeParameter r -> AlgorithmParameter r
ivy tree = AlgorithmParameter
  { algorithmName = "Ivy"
  , algorithmInitialTree = tree
  , algorithmGet = \_ -> Arvy.ivy
  }

constantRing :: AlgorithmParameter r
constantRing = AlgorithmParameter
  { algorithmName = "Constant Ring"
  , algorithmInitialTree = Tree.semiCircles
  , algorithmGet = \n -> Arvy.constantRing (n `div` 2)
  }

half :: Tree.InitialTreeParameter r -> AlgorithmParameter r
half tree = AlgorithmParameter
  { algorithmName = "half node"
  , algorithmInitialTree = tree
  , algorithmGet = \_ -> Arvy.half
  }
