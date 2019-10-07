{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Polysemy
import Polysemy.Trace
import Data.Time
import Data.MonoTraversable
import Data.NonNull
import Arvy.Algorithm
import Data.Sequences

data Request a = Request
  { requestFrom :: Int
  , requestRoot :: Int
  , path :: a
  } deriving (Functor, Show)

traceRequests :: forall r x . Members '[Lift IO, Trace] r => ConduitT x x (Sem r) ()
traceRequests = do
  time <- lift $ sendM getCurrentTime
  go 0 time where
  go :: Int -> UTCTime -> ConduitT x x (Sem r) ()
  go k prev = await >>= \case
    Nothing -> lift $ trace "Done"
    Just event -> do
      let k' = k + 1
      time <- lift $ sendM getCurrentTime
      if time `diffUTCTime` prev > 1
        then do
          lift $ trace $ "[" ++ show k' ++ "]"
          yield event
          go k' time
        else do
          yield event
          go k' prev


hopCount :: (Monad m, MonoFoldable seq) => ConduitT seq Int m ()
hopCount = C.map olength


requestDists :: (Monad m, Element seq ~ Node, IsSequence seq) => Env -> ConduitT (NonNull seq) Double m ()
requestDists Env { envWeights = weights } = C.map (\seq ->
                                                     sum $ zipWith (\from to ->
                                                                      weights ! (from, to))
                                                     (otoList (Data.NonNull.init seq))
                                                     (otoList (Data.NonNull.tail seq)))

requestRatios :: (Monad m, Element seq ~ Node, IsSequence seq) => Env -> ConduitT (NonNull seq) Double m ()
requestRatios Env { envWeights = weights } = C.map (\seq ->
                                                     (/ weights ! ( Data.NonNull.head seq
                                                                  , Data.NonNull.last seq))
                                                     $ sum $ zipWith (\from to ->
                                                                      weights ! (from, to))
                                                     (otoList (Data.NonNull.init seq))
                                                     (otoList (Data.NonNull.tail seq)))

--requestRatio :: Monad m => Env -> ConduitT ArvyEvent Double m ()
--requestRatio Env { envWeights = weights } = collectRequests (\edge -> Sum (weights ! edge)) -- Collect requests by measuring the length of the edges they take
--  .| C.map (\(Request a b (Sum path)) -> if a == b then 1 else path / weights ! (a, b)) -- Calculate the ratio between request edge lengths and graph edge length
