module Arvy.Local where

import           Data.Array.IArray
import           Data.Array.MArray
import           Polysemy
import           Polysemy.State
import Arvy.Algorithm
import Polysemy.Trace
import Data.Array.Unboxed
import Pipes

-- | The type of our nodes (indices)
type Node = Int
-- | The type of a count of nodes
type NodeCount = Node
-- | The type of our edges
type Edge = (Node, Node)

-- | The type to represent graph weights in a complete graph with a certain array type @arr@
type GraphWeightsArr arr = arr Edge Weight
-- | The type to represent immutable graph weights
type GraphWeights = GraphWeightsArr UArray


-- | A rooted spanning tree, an array of nodes where each node either points to a successor signified with 'Just' or is the root node, signified with 'Nothing'
type RootedTree = UArray Node Node

{-# INLINABLE runRequests #-}
runRequests
  :: forall m r arr
  . ( Member (Lift m) r
    , MArray arr Node m )
  => arr Node Node
  -> (RootedTree -> Sem r Int)
  -> Int
  -> Producer Node (Sem r) ()
runRequests tree getRequest requestCount = go requestCount where
  go :: Int -> Producer Node (Sem r) ()
  go 0 = return ()
  go k = do
    immutableTree <- lift $ sendM $ freeze tree
    req <- lift $ getRequest immutableTree
    yield req
    go (k - 1)

{-# INLINABLE runLocalWeights #-}
-- | Run local weights with weights in a matrix and a current node
runLocalWeights :: GraphWeights -> Node -> Sem (LocalWeights Node ': r) a -> Sem r a
runLocalWeights weights src = interpret $ \case
  WeightTo dst -> return $ weights ! (src, forward dst)


-- | The type of events that happen during an arvy execution
data ArvyEvent
  = RequestTravel Node Node String -- ^ A request message traveled from a node to another. The 'String' contains the 'show' of the message
  | SuccessorChange Node Node -- ^ The successor for some node changed to some new one
  | StateChange Node String -- ^ The state at a node changed to a new value. The 'String' contains the 'show' of the new state
  | RequestMade Node -- ^ A node made a request for the token
  | RequestGranted Node Node GrantType -- ^ A request for a node has been granted by some node

-- | How the request was granted, either locally if the request was there already, or by another node
data GrantType = Local | Received

instance Show ArvyEvent where
  show (RequestTravel a b msg) = "[MSG] " ++ show a ++ " -> " ++ show b ++ " (" ++ msg ++ ")"
  show (SuccessorChange i newSucc) = "[SUCC] " ++ show i ++ " changed its successor to " ++ show newSucc
  show (StateChange i newState) = "[STATE] " ++ show i ++ " changed its state to " ++ show newState
  show (RequestMade i) = "[REQ] " ++ show i ++ " made a request"
  show (RequestGranted i _ Local) = "[GRANT] The request from " ++ show i ++ " was fulfilled, the token was already there\n"
  show (RequestGranted i src Received) = "[GRANT] The request from " ++ show i ++ " was fulfilled, the token came from " ++ show src ++ "\n"

{- |
Run an Arvy algorithm locally, taking requests as input and outputting the events that happen during execution. The output value 'Nothing' means that every request has been granted.

- @m@ is the underlying Monad to run in, either 'IO' or @'Control.Monad.ST.ST' s@, needed for mutating the tree state efficiently
- @sarr@ is the array type used for storing the algorithm-specific state for nodes. Is probably always going to be 'Data.Array.IO.IOArray' for when @m@ is 'IO' and @'Data.Array.ST.STArray' s@ for when @m@ is @'Control.Monad.ST.ST' s@
- @tarr@ is the array type used for storing the tree state, should probably be the same type used for @sarr@
- @r@ is the effect stack the Arvy algorithm requires to run, which might include 'Polysemy.Random.Random'ness or others
- @r'@ is the effect stack of the result, which in addition to having all the effects of @r@ also takes requests as 'Input' and 'Output's 'ArvyEvent's
-}
{-# INLINABLE runArvyLocal #-}
runArvyLocal
  :: forall m s sarr tarr r a
  . ( Members '[ Lift m, Trace ] r
    , MArray sarr s m
    , MArray tarr Node m )
  => GraphWeights -- ^ The graph weights, used for giving nodes access to their local weights
  -> tarr Node Node -- ^ The (initial) spanning tree, which this function modifies as requests are coming in. This is to allow request generators access to it from the outside.
  -> sarr Node s
  -> Arvy s r -- ^ The Arvy algorithm to run
  -> Pipe Node ArvyEvent (Sem r) a
runArvyLocal weights tree stateArray (Arvy inst) = runArvyLocal' inst where
  runArvyLocal' :: forall msg . ArvyInst msg s r -> Pipe Node ArvyEvent (Sem r) a
  runArvyLocal' ArvyInst { .. } = go where

    -- | The main loop which repeatedly takes a request as input and processes it on the current state
    {-# INLINE go #-}
    go :: Pipe Node ArvyEvent (Sem r) a
    go = await >>= \i -> do
      yield $ RequestMade i
      getSuccessor i >>= \successor -> if i == successor
        -- If the node that made the request has no successor, immediately grant
        then yield $ RequestGranted i i Local
        else do
          msg <- runNode i (arvyInitiate i successor)
          setSuccessor i i
          yield $ RequestTravel i successor (show msg)
          root <- send msg successor
          yield $ RequestGranted i root Received
      go
      where

    --  where

      -- | Send a message to some node and repeatedly applies the arvy algorithm until eventually the root node is found, which gets returned
      {-# INLINE send #-}
      send :: msg Node -> Node -> Pipe Node ArvyEvent (Sem r) Int
      send msg i = getSuccessor i >>= \successor -> if i == successor
        then do
          newSucc <- runNode i (arvyReceive msg i)
          setSuccessor i newSucc
          return i
        else do
          (newSucc, newMsg) <- runNode i (arvyTransmit msg i successor)
          setSuccessor i newSucc
          yield $ RequestTravel i successor (show newMsg)
          send newMsg successor

      {-# INLINE getSuccessor #-}
      -- | Convenience function for getting the successor to a node by reading from the tree array
      getSuccessor :: Node -> Pipe Node ArvyEvent (Sem r) Node
      getSuccessor i = lift $ sendM $ readArray tree i

      {-# INLINE setSuccessor #-}
      -- | Convenience function for setting the successor to a node by writing to the tree array
      setSuccessor :: Node -> Node -> Pipe Node ArvyEvent (Sem r) ()
      setSuccessor i newSucc = do
        lift $ sendM $ writeArray tree i newSucc
        --output $ Just $ SuccessorChange i newSucc

      {-# INLINE runNodeState #-}
      -- | Interprets the state in a node as array operations to the state array at that nodes index
      runNodeState :: Node -> Sem (State s ': r) x -> Pipe Node ArvyEvent (Sem r) x
      runNodeState i = lift . interpret \case
        Get -> sendM $ readArray stateArray i
        Put s -> do
          sendM $ writeArray stateArray i s
          --output $ Just $ StateChange i (show s)

      {-# INLINE runNode #-}
      -- | Runs a node with its local effects
      runNode :: Node -> Sem (LocalWeights Node ': State s ': r) x -> Pipe Node ArvyEvent (Sem r) x
      runNode i action = runNodeState i (runLocalWeights weights i action)
