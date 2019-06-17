{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Arvy.Algorithm
  ( module Arvy.Algorithm
  , module Polysemy.State
  ) where

import           Arvy.Weights
import           Data.Array.MArray
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.Trace
import           Polysemy.State

{- |
An Arvy algorithm instance.
- @msg@ stands for the message type it sends between nodes, this can be arbitrarily chosen by the algorithm. It is parametrized by @i@ such that you can send node indices over messages.
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
  { arvyNodeInit :: forall i . NodeIndex i => i -> ArvySem r s
  -- ^ How to compute the initial state in nodes
  , arvyInitiate :: forall i . NodeIndex i => i -> ArvySem (State s ': r) (msg i)
  -- ^ Initial request message contents
  , arvyTransmit :: forall ir i . Forwardable ir i => i -> msg ir -> ArvySem (State s ': r) (ir, msg i)
  -- ^ What to do when a message passes through this node, what new successor to choose and what message to forward
  -- @ir@ stands for the node index type you received and need to select, which can be forwarded (with 'forward') to @i@ which is the node index type of the current node and the message to send
  , arvyReceive :: forall ir i . Forwardable ir i => i -> msg ir -> ArvySem (State s ': r) ir
  -- ^ What to do when a message arrives at the node holding the token, what new successor to choose.
  -- @ir@ stands for the node index type you received and need to select, which can be forwarded (with 'forward') to @i@ which is the node index type of the current node and the message to send
  }

-- | A convenience wrapper for the effects accessible in an Arvy algorithm. @l@ is a list of additional effects that should be available, @a@ is the result type
type ArvySem r a = Sem (LocalWeights ': r) a

-- | An existential wrapper for an Arvy algorithm. This allows us to have the same type for algorithms that use different @msg@ and @s@ types, and enforcing our Arvy runners implementation to be agnostic to these types. With this we can even loop through a list of @[Arvy]@ values, for e.g. testing each of them.
data Arvy r = forall msg s . Arvy (ArvyInst msg s r)

-- | Convenience function for flipping the type argument order of Arvy from @r@, @msg@, @s@ to @msg@, @s@, @r@
arvy :: ArvyInst msg s r -> Arvy r
arvy = Arvy

-- | A class for abstract node indices that can be converted to an 'Int'.
class NodeIndex i where
  -- | Extract the 'Int' value from the given abstract node index
  indexValue :: i -> Int
  default indexValue :: Integral i => i -> Int
  indexValue = fromIntegral
  
-- | A class for node indices that can be forwarded in one direction. Having this class as a constraint on types @i@ and @i'@ is equivalent to passing a function @i -> i'@.
class (NodeIndex ia, NodeIndex ib) => Forwardable ia ib where
  -- | Forward a node index
  forward :: ia -> ib
  default forward :: ia ~ ib => ia -> ib
  forward = id

instance NodeIndex Int
instance Forwardable Int Int

-- TODO: Use mono-traversable for safety and speedup
-- | A function for constructing an Arvy algorithm with just a function that selects the node to connect to out of a list of available ones.
simpleArvy :: forall r . (forall i . NodeIndex i => [i] -> ArvySem (State () ': r) i) -> Arvy r
simpleArvy selector = Arvy @r @[] @() ArvyInst
  { arvyNodeInit = \_ -> return ()
  , arvyInitiate = \i -> return [i]
  , arvyTransmit = \i msg -> do
      s <- selector msg
      return (s, i : fmap forward msg)
  , arvyReceive = \_ msg ->
      selector msg
  }


data ArvyEvent
  = RequestTravel Int Int String
  | SuccessorChange Int (Maybe Int)
  | StateChange Int String
  | RequestMade Int
  | RequestGranted GrantType

data GrantType
  = AlreadyHere Int
  | GottenFrom Int Int

instance Show ArvyEvent where
  show (RequestTravel a b msg) = "[MSG] " ++ show a ++ " -> " ++ show b ++ " (" ++ msg ++ ")"
  show (SuccessorChange i newSucc) = "[SUCC] " ++ show i ++ " changed its successor to " ++ show newSucc
  show (StateChange i newState) = "[STATE] " ++ show i ++ " changed its state to " ++ show newState
  show (RequestMade i) = "[REQ] " ++ show i ++ " made a request"
  show (RequestGranted (AlreadyHere i)) = "[GRANT] The request from " ++ show i ++ " was fulfilled, the token was already there"
  show (RequestGranted (GottenFrom i src)) = "[GRANT] The request from " ++ show i ++ " was fulfilled, the token came from " ++ show src

-- TODO: Make it take inputs 'Int' instead of 'Maybe Int', because it returns no result, we just rely on the outputs to get results -> Can control request count from the caller
{- |
Run an Arvy algorithm locally, taking requests as input and outputting the edges where requests travel through.

- @m@ is the underlying Monad to run in, either 'IO' or @'Control.Monad.ST.ST' s@, needed for mutating the tree state efficiently
- @sarr@ is the array type used for storing the algorithm-specific state for nodes. Is probably always going to be 'Data.Array.IO.IOArray' for when @m@ is 'IO' and @'Data.Array.ST.STArray' s@ for when @m@ is @'Control.Monad.ST.ST' s@
- @tarr@ is the array type used for storing the tree state, should probably be the same type used for @sarr@
- @r@ is the effect stack the Arvy algorithm requires to run, which might include 'Polysemy.Random.Random'ness or others
- @r'@ is the effect stack of the result, which in addition to having all the effects of @r@ also takes requests as 'Input' and 'Output's 'ArvyEvent's
-}
runArvyLocal
  :: forall m sarr tarr r r'
  . ( Members '[ Lift m, Trace ] r
      -- r represents the input effects, the one the Arvy algorithm needs. r' represents the output effects, which are the ones the algorithm needs plus it taking inputs and sending outputs. Lift and Trace are not included in these additional effects because the algorithm might need those itself.
    , r' ~ (Input (Maybe Int) ': Output ArvyEvent ': r)
      -- Quantified constraint because we need a mutable array to store the state of the nodes of /any/ possible algorithm, and every algorithm can choose its own state type
    , forall s . MArray sarr s m
    , MArray tarr (Maybe Int) m )
  => GraphWeights -- ^ The graph weights, used for giving nodes access to their local weights
  -> tarr Int (Maybe Int) -- ^ The (initial) spanning tree, which this function modifies as requests are coming in. This is to allow request generators access to it from the outside.
  -> Arvy r -- ^ The Arvy algorithm to run
  -> Sem r' ()
runArvyLocal weights tree (Arvy inst) = runArvyLocal' inst where
  runArvyLocal' :: forall msg s . ArvyInst msg s r -> Sem r' ()
  runArvyLocal' ArvyInst { .. } = do
    bounds <- sendM @m $ getBounds tree
    states <- traverse initiateState (range bounds)
    stateArray <- sendM @m $ newListArray bounds states
    go stateArray
    
    where

    initiateState :: Int -> Sem r' s
    initiateState i = raise . raise $ runLocalWeights weights i (arvyNodeInit i)

    go :: sarr Int s -> Sem r' ()
    go state = input >>= \case
      Nothing -> return ()
      Just i -> do
        output $ RequestMade i
        getSuccessor i >>= \case
          Nothing -> output $ RequestGranted (AlreadyHere i)
          Just successor -> do
            msg <- runNode i (arvyInitiate i)
            setSuccessor i Nothing
            output $ RequestTravel i successor (show msg)
            root <- send msg successor
            output $ RequestGranted (GottenFrom i root)
        go state
        
      where

      -- | Simulate sending some message to a node
      send :: msg Int -> Int -> Sem r' Int
      send msg i = getSuccessor i >>= \case
        Just successor -> do
          (newSucc, newMsg) <- runNode i (arvyTransmit i msg)
          setSuccessor i (Just newSucc)
          output $ RequestTravel i successor (show newMsg)
          send newMsg successor
        Nothing -> do
          newSucc <- runNode i (arvyReceive i msg)
          setSuccessor i (Just newSucc)
          return i

      getSuccessor :: Int -> Sem r' (Maybe Int)
      getSuccessor i = sendM @m (readArray tree i)

      setSuccessor :: Int -> Maybe Int -> Sem r' ()
      setSuccessor i newSucc = do
        output $ SuccessorChange i newSucc
        sendM @m (writeArray tree i newSucc)

      runNodeState :: Int -> Sem (State s ': r) a -> Sem r' a
      runNodeState i = raise . reinterpret \case
        Get -> sendM @m (readArray state i)
        Put s -> do
          output $ StateChange i (show s)
          sendM @m (writeArray state i s)

      runNode :: Int -> Sem (LocalWeights ': State s ': r) a -> Sem r' a
      runNode i action = runNodeState i (runLocalWeights weights i action)
