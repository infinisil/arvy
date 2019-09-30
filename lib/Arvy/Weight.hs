{-# LANGUAGE TemplateHaskell #-}

module Arvy.Weight where

import           Arvy.Algorithm
import           Polysemy
import           Polysemy.State

type Weight = Double

data Weights ib (m :: * -> *) a where
  WeightTo :: Forwardable ia ib => ia -> Weights ib m Weight

makeSem ''Weights

{-# INLINE weightHandler #-}
weightHandler
  :: Member (State a) r
  => (a -> i -> Weight)
  -> Weights i m x -> Sem r x
weightHandler f = \case
  WeightTo i -> do
    a <- get
    return $ f a (forward i)
