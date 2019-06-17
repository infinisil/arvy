module Arvy.Algorithm.Ivy where

import           Arvy.Algorithm

newtype IvyMessage i = IvyMessage i deriving Show

ivy :: Arvy r
ivy = arvy @IvyMessage @() ArvyInst
  { arvyNodeInit = \_ -> return ()
  , arvyInitiate = return . IvyMessage
  , arvyTransmit = \_ (IvyMessage root) ->
      return (root, IvyMessage (forward root))
  , arvyReceive = \_ (IvyMessage root) ->
      return root
  }
