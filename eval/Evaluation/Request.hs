{-# LANGUAGE DeriveFunctor     #-}
module Evaluation.Request where

import Arvy.Local

import Data.Monoid
import Data.Functor
import Data.Array.Unboxed
import Prelude hiding ((.))
import Control.Category
import Conduit
import qualified Data.Conduit.Combinators as C
import Evaluation.Types

data Request a = Request
  { requestFrom :: Int
  , requestRoot :: Int
  , path :: a
  } deriving (Functor, Show)

collectRequests :: forall a m . (Monoid a, Monad m) => (Edge -> a) -> ConduitT ArvyEvent (Request a) m ()
collectRequests f = go 0 mempty where
  go :: Int -> a -> ConduitT ArvyEvent (Request a) m ()
  go requestFrom as = await >>= \case
    Just (RequestMade requestFrom') -> go requestFrom' mempty
    Just (RequestGranted _ root _) -> do
      yield (Request requestFrom root as)
      go 0 mempty
    Just (RequestTravel x y _) -> go requestFrom (f (x, y) <> as)
    Just _ -> go requestFrom as
    Nothing -> return ()

hopCount :: (Num n, Monad m) => ConduitT ArvyEvent n m ()
hopCount = collectRequests (\_ -> Sum 1) .| C.map (getSum . path)

ratio :: Monad m => Env -> ConduitT ArvyEvent Double m ()
ratio Env { envWeights = weights } = collectRequests (\edge -> Sum (weights ! edge)) -- Collect requests by measuring the length of the edges they take
  .| C.filter (\(Request a b _) -> a /= b) -- Only look at requests where start node != end node
  .| C.map (\(Request a b (Sum path)) -> path / weights ! (a, b)) -- Calculate the ratio between request edge lengths and graph edge length
