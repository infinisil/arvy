{-# LANGUAGE DeriveFunctor     #-}
module Evaluation.Request where

import Arvy.Local

import Data.Monoid
import Data.Functor
import Data.Array.Unboxed
import Prelude hiding ((.))
import Control.Category
import Pipes
import qualified Pipes.Prelude as P

data Request a = Request
  { requestFrom :: Int
  , requestRoot :: Int
  , path :: a
  } deriving (Functor, Show)

requests :: forall a m x . (Monoid a, Monad m) => (Edge -> a) -> Pipe ArvyEvent (Request a) m x
requests f = go 0 mempty where
  go :: Int -> a -> Pipe ArvyEvent (Request a) m x
  go requestFrom as = await >>= \case
    RequestMade requestFrom' -> go requestFrom' mempty
    RequestGranted _ root _ -> do
      yield (Request requestFrom root as)
      go 0 mempty
    RequestTravel x y _ -> go requestFrom (f (x, y) <> as)
    _ -> go requestFrom as

hopCount :: (Num n, Monad m) => Pipe ArvyEvent n m x
hopCount = requests (\_ -> Sum 1) >-> P.map (getSum . path)

ratio :: Monad m => GraphWeights -> Pipe ArvyEvent Double m x
ratio weights = requests (\edge -> Sum (weights ! edge)) -- Collect requests by measuring the length of the edges they take
  >-> P.filter (\(Request a b _) -> a /= b) -- Only look at requests where start node != end node
  >-> P.map (\(Request a b (Sum path)) -> path / weights ! (a, b)) -- Calculate the ratio between request edge lengths and graph edge length
