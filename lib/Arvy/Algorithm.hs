{-# LANGUAGE TemplateHaskell #-}

module Arvy.Algorithm
  ( module Arvy.Algorithm
  , module Polysemy.State
  ) where

import Polysemy
import Polysemy.State

{- |
An Arvy algorithm instance.
- @msg@ stands for the message type it sends between nodes, this can be arbitrarily chosen by the algorithm. It is parametrized by @i@ such that the algorithm can send node indices over messages.
- @s@ stands for the state type nodes can store, this can be arbitrarily chose by the algorithm.
- @i@ stands for the node index type, which can be passed via messages and be has to be used to select which node to choose as the next successor.

- Before the algorithm runs, every node's state gets initialized by calling 'arvyNodeInit' with its index.
- When a node initiates a request, 'arvyInitiate' is called to generate the initial message.
- This message gets passed along the path to the current token holder. For nodes the request travels /through/, 'arvyTransmit' is called, which has to select a node index to set as a new successor and has to return a new message to forward.
- When the request reaches the final node holding the token, 'arvyReceive' is called, which has to select a node index to set as a new successor.

All methods have access to the node's index they run in and the weights to all neighboring nodes. In addition, every method except 'arvyNodeInit' has read and write access to the current node's state.

The types @i@ and @ir@ are used to ensure the algorithms correctness, in that they are chosen such that you can only select a new successor from already traveled through nodes.
-}
data ArvyInst msg s r = (forall i . Show i => Show (msg i), Show s) => ArvyInst
  { arvyNodeInit :: forall i . NodeIndex i => i -> Sem (LocalWeights ': r) s
  -- ^ How to compute the initial state in nodes
  , arvyInitiate :: forall i . NodeIndex i => i -> Sem (LocalWeights ': State s ': r) (msg i)
  -- ^ Initial request message contents
  , arvyTransmit :: forall ir i . Forwardable ir i => i -> msg ir -> Sem (LocalWeights ': State s ': r) (ir, msg i)
  -- ^ What to do when a message passes through this node, what new successor to choose and what message to forward
  -- @ir@ stands for the node index type you received and need to select, which can be forwarded (with 'forward') to @i@ which is the node index type of the current node and the message to send
  , arvyReceive :: forall ir i . Forwardable ir i => i -> msg ir -> Sem (LocalWeights ': State s ': r) ir
  -- ^ What to do when a message arrives at the node holding the token, what new successor to choose.
  -- @ir@ stands for the node index type you received and need to select, which can be forwarded (with 'forward') to @i@ which is the node index type of the current node and the message to send
  }

-- | An existential wrapper for an Arvy algorithm. This allows us to have the same type for algorithms that use different @msg@ and @s@ types, and enforcing our Arvy runners implementation to be agnostic to these types. With this we can even loop through a list of @['Arvy']@ values, for e.g. testing each of them.
data Arvy r = forall msg s . Arvy (ArvyInst msg s r)

-- | Convenience function for flipping the type argument order of 'Arvy' from @r@, @msg@, @s@ to @msg@, @s@, @r@
arvy :: ArvyInst msg s r -> Arvy r
arvy = Arvy

-- | A class for abstract node indices that can be converted to an 'Int'.
class NodeIndex i where
  -- | Extract the 'Int' value from the given abstract node index
  indexValue :: i -> Int
  
-- | A class for node indices that can be forwarded in one direction. Having this class as a constraint on types @i@ and @i'@ is equivalent to passing a function @i -> i'@.
class (NodeIndex ia, NodeIndex ib) => Forwardable ia ib where
  -- | Forward a node index
  forward :: ia -> ib

instance NodeIndex Int where
  indexValue = id
  
instance Forwardable Int Int where
  forward = id

  
-- | The type to use for edge weights
type Weight = Double

-- TODO: Use abstract i type
-- | An effect for providing access to weights from a current node to others
data LocalWeights (m :: * -> *) a where
  WeightTo :: Int -> LocalWeights m Weight

makeSem ''LocalWeights

-- TODO: Use mono-traversable for safety and speedup
-- | A function for constructing an Arvy algorithm with just a function that selects the node to connect to out of a list of available ones.
simpleArvy :: (forall i . NodeIndex i => [i] -> Sem (LocalWeights ': State () ': r) i) -> Arvy r
simpleArvy selector = arvy @[] @() ArvyInst
  { arvyNodeInit = \_ -> return ()
  , arvyInitiate = \i -> return [i]
  , arvyTransmit = \i msg -> do
      s <- selector msg
      return (s, i : fmap forward msg)
  , arvyReceive = \_ msg ->
      selector msg
  }

