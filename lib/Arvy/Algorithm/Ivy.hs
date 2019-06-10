module Arvy.Algorithm.Ivy where

import           Arvy.Algorithm

newtype IvyMessage i = IvyMessage i

ivy :: Arvy r
ivy = Arvy
  { arvyNodeInit = \_ -> return ()
  , arvyInitiate = return . IvyMessage
  , arvyTransmit = \_ (IvyMessage root) ->
      return (root, IvyMessage (forward root))
  , arvyReceive = \_ (IvyMessage root) ->
      return root
  }
