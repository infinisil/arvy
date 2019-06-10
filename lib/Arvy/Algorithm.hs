{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE QuantifiedConstraints       #-}
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

module Arvy.Algorithm
  ( module Arvy.Algorithm
  , module Polysemy.State
  ) where

import           Arvy.Weights
import           Arvy.Tree
import           Arvy.Requests
import           Data.Array.MArray
import           Data.Array.IO
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.Trace
import           Polysemy.State
import           Polysemy.Random

{- |

An Arvy algorithm, a combination between Arrow and Ivy.
@r@ stands for the effects it runs in, chosen by the caller, but the algorithm can enforce certain constraints on it, like requiring randomness.
@msg@ stands for the message type it sends between nodes, this can be arbitrarily chosen by the algorithm. It is parametrized by @i@ such that you can send node indices over messages.
@s@ stands for the state type nodes can store, this can be arbitrarily chose by the algorithm.
@i@ stands for the node index type, which can be passed via messages and be has to be used to select which node to choose as the next successor.

- Before the algorithm runs, every node's state gets initialized by calling 'arvyNodeInit' with its index.
- When a node initiates a request, 'arvyInitiate' is called to generate the initial message.
- This message gets passed along the path to the current token holder. For nodes the request travels *through*, 'arvyTransmit' is called, which has to select a node index to set as a new successor and has to return a new message to forward.
- When the request reaches the final node holding the token, 'arvyReceive' is called, which has to select a node index to set as a new successor.

All methods have access to the node's index they run in and the weights to all other nodes. In addition, every method except 'arvyNodeInit' has read and write access to the current node's state.

The types @i@ and @i'@ are used to ensure the algorithms correctness, in that they are chosen such that you can only select a new successor from already traveled through nodes.

-}
data Arvy r = forall msg s . Arvy
  { arvyNodeInit :: forall i . NodeIndex i => i -> Sem (LocalWeights ': r) s
  -- ^ How to compute the initial state in nodes
  , arvyInitiate :: forall i . NodeIndex i => i -> Sem (LocalWeights ': State s ': r) (msg i)
  -- ^ Initial request message contents
  , arvyTransmit :: forall ir i . Forwardable ir i => i -> msg ir -> Sem (LocalWeights ': State s ': r) (ir, msg i)
  -- ^ What to do when a message passes through this node, what new successor to choose and what message to forward
  -- @ir@ stands for the node index type you received and need to select, which can be forwarded to @i@ which is the node index type of the current node and the message to send
  , arvyReceive :: forall ir i . Forwardable ir i => i -> msg ir -> Sem (LocalWeights ': State s ': r) ir
  -- ^ What to do when a message arrives at the node holding the token, what new successor to choose.
  -- @ir@ stands for the node index type you received and need to select, which can be forwarded to @i@ which is the node index type of the current node and the message to send
  }

-- | A class for abstract node indices that can be converted to an 'Int'.
class NodeIndex i where
  -- | Extract the 'Int' value from the given abstract node index
  indexValue :: i -> Int
  default indexValue :: Integral i => i -> Int
  indexValue = fromIntegral
  
-- | A class for node indices that can be forwarded in one direction. Having this class as a constraint on types @i@ and @i'@ is equivalent to passing a function @i -> i'@.
class (NodeIndex i, NodeIndex i') => Forwardable i i' where
  -- | Forward a node index
  forward :: i -> i'
  default forward :: i ~ i' => i -> i'
  forward = id

instance NodeIndex Int
instance Forwardable Int Int


-- TODO: Use mono-traversable for speedup
superSimpleArvy :: (forall i . [i] -> Sem r i) -> Arvy r
superSimpleArvy selector = Arvy
  { arvyNodeInit = \_ -> return ()
  , arvyInitiate = \i -> return [i]
  , arvyTransmit = \i msg -> do
      s <- raise . raise $ selector msg
      return (s, i : fmap forward msg)
  , arvyReceive = \_ msg ->
      raise . raise $ selector msg
  }


runArvyLocal
  :: forall m narr r tarr
  . ( Members '[ Input (Maybe Int)
               , Lift m
               , Trace
               , Output (Int, Int) ] r
    , MArray tarr (Maybe Int) m
    , forall s . MArray narr s m )
  => Int
  -> GraphWeights
  -> tarr Int (Maybe Int)
  -> Arvy r
  -> Sem r ()
runArvyLocal count weights tree Arvy { .. } = runArvyLocal' arvyNodeInit arvyInitiate arvyTransmit arvyReceive where
  runArvyLocal'
    :: forall msg s
     . (Int -> Sem (LocalWeights ': r) s)
    -> (Int -> Sem (LocalWeights ': State s ': r) (msg Int))
    -> (Int -> msg Int -> Sem (LocalWeights ': State s ': r) (Int, msg Int))
    -> (Int -> msg Int -> Sem (LocalWeights ': State s ': r) Int)
    -> Sem r ()
  runArvyLocal' nodeInit initiate transmit receive = do
    states <- traverse (\i -> runLocalWeights weights i (nodeInit i)) [0 .. count - 1]
    stateArray <- sendM @m $ newListArray (0, count - 1) states
    go stateArray
    
    where
      
    go :: narr Int s -> Sem r ()
    go state = input >>= \case
      Nothing -> trace "All requests fulfilled"
      Just i -> do
        trace $ "Request from node " ++ show i
        getSuccessor i >>= \case
          Nothing -> trace "This node already has the token"
          Just successor -> do
            msg <- runNode i (initiate i)
            setSuccessor i Nothing
            output (i, successor)
            send msg successor
        go state
        
      where
        
      send :: msg Int -> Int -> Sem r ()
      send msg i = getSuccessor i >>= \case
        Nothing -> do
          newSucc <- runNode i (receive i msg)
          setSuccessor i (Just newSucc)
        Just successor -> do
          (newSucc, newMsg) <- runNode i (transmit i msg)
          setSuccessor i (Just newSucc)
          output (i, successor)
          send newMsg successor

      getSuccessor :: Int -> Sem r (Maybe Int)
      getSuccessor i = sendM @m (readArray tree i)

      setSuccessor :: Int -> Maybe Int -> Sem r ()
      setSuccessor i newSucc = sendM @m (writeArray tree i newSucc)

      runNodeState :: Int -> Sem (State s ': r) a -> Sem r a
      runNodeState i = interpret $ \case
        Get -> sendM @m (readArray state i)
        Put s -> sendM @m (writeArray state i s)

      runNode :: Int -> Sem (LocalWeights ': State s ': r) a -> Sem r a
      runNode i action = runNodeState i (runLocalWeights weights i action)
