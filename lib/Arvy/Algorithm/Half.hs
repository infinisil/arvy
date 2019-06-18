module Arvy.Algorithm.Half where

import Arvy.Algorithm


half :: Arvy r
half = arvy @[] @() ArvyInst
  { arvyNodeInit = \_ -> return ()
  , arvyInitiate = \i -> return [i]
  , arvyTransmit = \i msg -> return (middle msg, i : fmap forward msg)
  , arvyReceive = \_ msg -> return (middle msg)
  } where
  middle xs = xs !! (length xs `div` 2)
