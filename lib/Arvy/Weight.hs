{-# LANGUAGE TemplateHaskell #-}

module Arvy.Weight where

import           Arvy.Algorithm
import           Polysemy

type Weight = Double

data LocalWeights ib (m :: * -> *) a where
  WeightTo :: Forwardable ia ib => ia -> LocalWeights ib m Weight

makeSem ''LocalWeights

{-# INLINE weightHandler #-}
weightHandler
  :: (i -> Weight)
  -> LocalWeights i m x -> Sem r x
weightHandler f = \case
  WeightTo i -> return $ f (forward i)

class HasWeights a where
  getWeights :: a -> Node -> Weight
