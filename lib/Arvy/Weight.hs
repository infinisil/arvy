{-# LANGUAGE TemplateHaskell #-}

module Arvy.Weight where

import           Arvy.Algorithm
import           Polysemy

type Weight = Double

data Weights ib (m :: * -> *) a where
  WeightTo :: Forwardable ia ib => ia -> Weights ib m Weight

makeSem ''Weights

{-# INLINE weightHandler #-}
weightHandler
  :: (i -> Weight)
  -> Weights i m x -> Sem r x
weightHandler f = \case
  WeightTo i -> return $ f (forward i)

class HasWeights a where
  getWeights :: a -> Node -> Weight
