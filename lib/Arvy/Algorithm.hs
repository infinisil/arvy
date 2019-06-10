{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Arvy.Algorithm where

import           Polysemy
import           Polysemy.Reader
import           Polysemy.State


{- |
An Arvy algorithm, a combination between Arrow and Ivy.
@r@ stands for the effects it runs in, chosen by the caller, but the algorithm can enforce certain constraints on it, like requiring randomness.
@msg@ stands for the message type it sends between nodes, this can be arbitrarily chosen by the algorithm.
@i@ stands for the node index type, which can be passed via messages and be has to be used to select which node to choose as the next successor.

- When a node @x@ initiates a request, 'arvyInitiate' is called to generate the initial message, which has access to the current node's index through a 'Reader i' effect.
- This message gets passed along the path from @x@ to the current token holder @y@. For nodes the request travels *through*, 'arvyTransmit' is called, which can transform the message, potentially adding a reference to the current node's index.
- Whenever a message arrives at a node, 'arvySelect' is called, which has to select a node index from the received message. For this selection, the algorithm does *not* have access to the current node's index. This is to ensure correctness of it, because only *previously* traversed nodes should be selected.

-}

data Env i = Env
  { nodeIndex      :: i
  , nodeCount      :: Word
  , neigborWeights :: Word -> Double
  }

data Arvy r = forall msg s . Arvy
  { arvyNodeInit :: forall i . ToIx i => Sem (Reader (Env i) ': r) s
  -- ^ Initial state in all nodes
  -- ACcessible is the index of the node
  , arvyInitiate :: forall i . ToIx i => Sem (State s ': Reader (Env i) ': r) (msg i)
  -- ^ Initial request message contents
  -- Accessible is the state of the node that sends the message and the index of it
  , arvyTransmit :: forall i' i . (TIx i' i) => msg i' -> Sem (State s ': Reader (Env i) ': r) (i', msg i)
  -- ^ How to transmit a message when a node received one and has to pass it on
  -- Accessible is the state of the node that receive the mess
  , arvyReceive :: forall i' i . (TIx i' i) => msg i' -> Sem (State s ': Reader (Env i) ': r) i'


-- ^ How to determine the new successor from a received message, this is the meat of an Arvy algorithm
  -- Accessible is the state of the node that received the message and the index of it
  -- However you can't use the index of the current node as a return value in order to ensure correctness of Arvy.
  }

newtype ArrowMessage i = ArrowMessage i




arrow :: Arvy r
arrow = Arvy
  { arvyNodeInit = return ()
  , arvyInitiate =
      ArrowMessage <$> asks nodeIndex
  , arvyTransmit = \(ArrowMessage sender) -> do
      i <- asks nodeIndex
      return (sender, ArrowMessage i)
  , arvyReceive = \(ArrowMessage sender) ->
      return sender
  }

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

data RingNodeState
  = SemiNode
  | BridgeNode

constantRing :: forall r . Word -> Arvy r
constantRing firstBridge = Arvy
  { arvyNodeInit = initial
  , arvyInitiate = do
      i <- asks nodeIndex
      get >>= \case
        SemiNode -> return (BeforeCrossing i i)
        BridgeNode -> do
          put SemiNode
          return (Crossing i)
  , arvyTransmit = \case
      BeforeCrossing { root, sender } -> get >>= \case
        SemiNode -> do
          i <- asks nodeIndex
          return (sender, BeforeCrossing (forwarded root) i)
        BridgeNode -> do
          put SemiNode
          return (sender, Crossing (forwarded root))
      Crossing { root } -> do
        put BridgeNode
        i <- asks nodeIndex
        return (root, AfterCrossing i)
      AfterCrossing { sender } -> do
        i <- asks nodeIndex
        return (sender, AfterCrossing i)
  , arvyReceive = \case
      BeforeCrossing { sender } -> return sender
      Crossing { root } -> do
        put BridgeNode
        return root
      AfterCrossing { sender } -> return sender
  }
  where
    initial :: forall i . ToIx i => Sem (Reader (Env i) ': r) RingNodeState
    initial = do
      value <- asks @(Env i) (toIx . nodeIndex)
      return $ if value == firstBridge
        then BridgeNode
        else SemiNode

--newtype IvyMessage i = IvyMessage i
--
--ivy :: Arvy r
--ivy = Arvy
--  { arvyNodeInit = return ()
--  , arvyInitiate =
--      IvyMessage <$> ask
--  , arvyTransmit =
--      return
--  , arvySelect = \(IvyMessage root) ->
--      return root
--  }

class (ToIx a, ToIx b) => TIx a b where
  forwarded :: a -> b
  default forwarded :: a ~ b => a -> b
  forwarded = id

class ToIx a where
  toIx :: a -> Word

instance ToIx Word where
  toIx = id

--
--runArvyLocal :: forall i r . Members '[Trace, SpanningTree i, Output (i, i)] r => Arvy r -> i -> Sem r ()
--runArvyLocal (Arvy n s t rr) = runArvyLocal' (n, s, t, rr) where
--  runArvyLocal' :: forall i r msg n
--    . Members '[Trace, SpanningTree i, Output (i, i)] r
--    => ( Sem (Reader i ': r) n
--       , Sem (State n ': Reader i ': r) (msg i)
--       , msg i -> Sem (State n ': Reader i ': r) (msg i)
--       , msg i -> Sem (State n ': r) i
--       )
--    -> i
--    -> Sem r ()
--  runArvyLocal' (nodeInit, send, transfer, select) r = do
--    trace "Initiating request"
--    getSuccessor r >>= \case
--      Nothing -> trace "Token is already here\n"
--      Just n -> do
--        setSuccessor r Nothing
--        (_, msg) <- runReader r (runState undefined send)
--        output (r, n)
--        go msg n
--    where
--      go :: msg i -> i -> Sem r ()
--      go msg r = do
--        (_, nextSucc) <- runState undefined $ select msg
--        currentSucc <- getSuccessor r
--        setSuccessor r (Just nextSucc)
--        case currentSucc of
--          Nothing ->
--            trace "Request arrived at token holder\n"
--          Just n -> do
--            (_, newMsg) <- runReader r $ runState undefined $ transfer msg
--            output (r, n)
--            go newMsg n


-- TODO: Use mono-traversable for speedup
--fromSelection :: (forall i . [i] -> Sem (State () ': r) i) -> Arvy r
--fromSelection select = Arvy
--  { arvyNodeInit = return ()
--  , arvyInitiate = do
--      i <- ask
--      return [i]
--  , arvyTransmit = \msg -> do
--      i <- ask
--      return $ i : msg
--  , arvySelect = select
--  }

--arrow' :: Arvy r
--arrow' = fromSelection (return . head)
--
--ivy' :: Arvy r
--ivy' = fromSelection (return . last)

