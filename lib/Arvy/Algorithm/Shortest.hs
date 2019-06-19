module Arvy.Algorithm.Shortest where

import Arvy.Algorithm
import Data.Bifunctor
import Control.Monad
import Arvy.Weights
import Data.List
import Data.Ord

newtype ShortestMessage i = ShortestMessage [(i, Double)] deriving Show

shortest :: Arvy r
shortest = arvy @ShortestMessage @() ArvyInst
  { arvyNodeInit = \_ -> return ()
  , arvyInitiate = \i -> do
      return $ ShortestMessage [(i, 0)]
  , arvyTransmit = \i (ShortestMessage msg) -> do
      dists <- forM msg $ \(o, w) -> do
        w' <- weightTo (indexValue o)
        let total = w + w'
        return (o, total)
      let (mn, m) = maximumBy (comparing snd) dists
      return (mn, ShortestMessage $ (i, m) : map (first forward) msg)
  , arvyReceive = \i (ShortestMessage msg) -> do
      dists <- forM msg $ \(o, w) -> do
        w' <- weightTo (indexValue o)
        let total = w + w'
        return (o, total)
      let (mn, m) = maximumBy (comparing snd) dists
      return mn
      
  }
