{-# LANGUAGE TypeApplications #-}

module Arvy.Algorithm.Arrow where

import           Arvy.Algorithm

newtype ArrowMessage i = ArrowMessage i

arrow :: Arvy
arrow = Arvy @ArrowMessage @() ArvyInst
  { arvyNodeInit = \_ -> return ()
  , arvyInitiate = return . ArrowMessage
  , arvyTransmit = \i (ArrowMessage sender) ->
      return (sender, ArrowMessage i)
  , arvyReceive = \_ (ArrowMessage sender) ->
      return sender
  }
