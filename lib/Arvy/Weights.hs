{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell     #-}
module Arvy.Weights where

import Algebra.Graph.AdjacencyIntMap hiding (edge)
import qualified Algebra.Graph.Class as G
import Arvy.Utils
import Control.Monad
import Control.Monad.Primitive
import Data.Array.ST
import Data.Array.Base
import Polysemy
import Data.Graph.Generators.Random.ErdosRenyi
import Data.Graph.Generators.Random.BarabasiAlbert (barabasiAlbertGraph)
import Algebra.Graph.ToGraph (toGraph)
import qualified Data.IntMultiSet as IntMultiSet
import Data.IntMultiSet (IntMultiSet)
import Polysemy.RandomFu
import Data.List (foldl')
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Random.Distribution.Uniform
import qualified Data.Random                   as R
import qualified Data.Random.Internal.Source   as R
import qualified Data.Random.Source.PureMT as R

import Data.Random.Source
import Data.Random.Source.MWC

-- | The type of our nodes (indices)
type Node = Int
-- | The type of a count of nodes
type NodeCount = Node
-- | The type of our edges
type Edge = (Node, Node)
-- | The type to use for edge weights
type Weight = Double

-- | The type to represent graph weights in a complete graph with a certain array type @arr@
type GraphWeightsArr arr = arr Edge Weight
-- | The type to represent immutable graph weights
type GraphWeights = GraphWeightsArr UArray

-- TODO: Use abstract i type
-- | An effect for providing access to weights from a current node to others
data LocalWeights (m :: * -> *) a where
  WeightTo :: Node -> LocalWeights m Weight

makeSem ''LocalWeights

-- | Run local weights with weights in a matrix and a current node
runLocalWeights :: GraphWeights -> Node -> Sem (LocalWeights ': r) a -> Sem r a
runLocalWeights weights src = interpret $ \case
  WeightTo dst -> return $ weights ! (src, dst)


  -- | Run a 'Random' effect using a given 'R.RandomSource'
runRandomSource'
  :: forall s r a m
   . (Member (Lift m) r, R.RandomSource m s)
  => s
  -> Sem (RandomFu ': r) a
  -> Sem r a
runRandomSource' source = interpret $ \case
    SampleRVar    rv -> sendM $ R.runRVar (R.sample rv) source
    GetRandomPrim pt -> sendM $ R.runRVar (R.getRandomPrim pt) source
{-# INLINEABLE runRandomSource' #-}
  
