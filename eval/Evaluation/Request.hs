{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Evaluation.Request where

import Arvy.Local
import Evaluation
import Evaluation.Utils

import Data.Monoid
import Data.Functor
import Data.Bifunctor
import Polysemy.State
import GHC.Generics
import Polysemy.Output
import Data.Array.Unboxed
import Prelude hiding ((.))
import Control.Category
import Control.DeepSeq

data Request a = Request
  { requestFrom :: Int
  , requestRoot :: Int
  , path :: a
  } deriving (Functor, Show)

requests :: forall a . Monoid a => (Edge -> a) -> Tracer ArvyEvent (Request a)
requests f = Tracer (0 :: Int, mempty :: a) \case
  Just (RequestMade requestFrom) -> put (requestFrom, mempty)
  Just (RequestGranted _ root _) -> do
    (requestFrom, as) <- get
    output $ Request requestFrom root as
  Just (RequestTravel x y _) -> do
    modify $ second (f (x, y) <>)
  _ -> return ()

hopCount :: Num n => Tracer ArvyEvent n
hopCount = requests (\_ -> Sum 1) <&> getSum . path

ratio :: GraphWeights -> Tracer ArvyEvent Double
ratio weights =
  ( requests (\edge -> Sum (weights ! edge)) -- Collect requests by measuring the length of the edges they take
  >>> filtering (\(Request a b _) -> a /= b) -- Only look at requests where start node != end node
  ) <&> \(Request a b (Sum path)) -> path / weights ! (a, b) -- Calculate the ratio between request edge lengths and graph edge length
