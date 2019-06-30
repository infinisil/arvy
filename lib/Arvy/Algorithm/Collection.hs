module Arvy.Algorithm.Collection
  ( arrow, ivy, half, constantRing
  ) where

import Arvy.Algorithm
import Data.Sequences
import Data.NonNull

newtype ArrowMessage i = ArrowMessage i deriving Show

-- | The Arrow Arvy algorithm, which always inverts all edges requests travel through. The shape of the tree therefore always stays the same
arrow :: Arvy r
arrow = arvy @ArrowMessage @() ArvyInst
  { arvyNodeInit = \_ -> return ()
  , arvyInitiate = return . ArrowMessage
  , arvyTransmit = \i (ArrowMessage sender) ->
      return (sender, ArrowMessage i)
  , arvyReceive = \_ (ArrowMessage sender) ->
      return sender
  }


newtype IvyMessage i = IvyMessage i deriving Show

-- | The Ivy Arvy algorithm, which always points all nodes back to the root node where the request originated from.
ivy :: Arvy r
ivy = arvy @IvyMessage @() ArvyInst
  { arvyNodeInit = \_ -> return ()
  , arvyInitiate = return . IvyMessage
  , arvyTransmit = \_ (IvyMessage root) ->
      return (root, IvyMessage (forward root))
  , arvyReceive = \_ (IvyMessage root) ->
      return root
  }

-- | An Arvy algorithm that always chooses the node in the middle of the traveled through path as the new successor.
half :: Arvy r
half = simpleArvy middle where
  middle xs = return $ xs' `unsafeIndex` (lengthIndex xs' `div` 2) where
    xs' = toNullable xs

data RingMessage i
  = BeforeCrossing
      { root   :: i
      , sender :: i
      }
  | Crossing
      { root   :: i
      }
  | AfterCrossing
      { sender :: i
      }
  deriving Show

data RingNodeState
  = SemiNode
  | BridgeNode
  deriving Show

-- TODO: Redesign parameters such that Arvy algorithms can specify an initial tree
-- | An Arvy algorithm that runs in constant competitive ratio on ring graphs. It works by splitting the ring into two semi-circles, connected by a bridge. The semi-circles are always kept intact, but whenever the bridge is traversed, the root node is selected as the new bridge end, while the previous bridge end becomes the new bridge start.
constantRing :: Int -> Arvy r
constantRing firstBridge = arvy @RingMessage @RingNodeState ArvyInst
  { arvyNodeInit = \i -> return $
    if indexValue i == firstBridge
        then BridgeNode
        else SemiNode

  , arvyInitiate = \i -> get >>= \case
      -- If our initial node is part of a semi-circle, the message won't be crossing the bridge yet if at all
      SemiNode -> return (BeforeCrossing i i)
      -- If our initial node is the bridge node, the message will travel accross the bridge, and our current node will become a semi-circle one
      BridgeNode -> do
        put SemiNode
        return (Crossing i)

  , arvyTransmit = \i -> \case
      BeforeCrossing { root, sender } -> get >>= \case
        -- If we haven't crossed the bridge yet, and the message traverses through another non-bridge node
        SemiNode ->
          return (sender, BeforeCrossing (forward root) i)
        -- If however we're the bridge node, we send a crossing message and make the current node a semi-circle one
        BridgeNode -> do
          put SemiNode
          return (sender, Crossing (forward root))
      Crossing { root } -> do
        -- If we received a message saying that the bridge was just crossed, make the current node the next bridge start
        put BridgeNode
        return (root, AfterCrossing i)
      AfterCrossing { sender } ->
        return (sender, AfterCrossing i)

  , arvyReceive = \_ -> \case
      BeforeCrossing { sender } -> return sender
      Crossing { root } -> do
        put BridgeNode
        return root
      AfterCrossing { sender } -> return sender
  }
