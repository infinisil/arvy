module Arvy.Local where

import           Data.Array.IArray
import           Data.Array.MArray
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import Arvy.Algorithm
import Polysemy.Output
import Polysemy.Trace
import Data.Array.Unboxed

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


-- TODO: Could be a lot faster with an UArray Node Node where a node pointing to itself represents a root node
-- | A rooted spanning tree, an array of nodes where each node either points to a successor signified with 'Just' or is the root node, signified with 'Nothing'
type RootedTree = Array Node (Maybe Node)

runRequests
  :: forall m r arr a
  . ( Member (Lift m) r
    , MArray arr (Maybe Int) m )
  => arr Int (Maybe Int)
  -> (Array Int (Maybe Int) -> Sem r Int)
  -> Int
  -> Sem (Input (Maybe Int) ': r) a
  -> Sem r a
runRequests tree getRequest requestCount =
  fmap snd . runState requestCount . reinterpret \case
    Input -> get >>= \case
      0 -> return Nothing
      k -> do
        put (k - 1)
        immutableTree <- sendM $ freeze tree
        Just <$> raise (getRequest immutableTree)
        
-- | Run local weights with weights in a matrix and a current node
runLocalWeights :: GraphWeights -> Node -> Sem (LocalWeights ': r) a -> Sem r a
runLocalWeights weights src = interpret $ \case
  WeightTo dst -> return $ weights ! (src, dst)


-- | The type of events that happen during an arvy execution
data ArvyEvent
  = RequestTravel Node Node String -- ^ A request message traveled from a node to another. The 'String' contains the 'show' of the message
  | SuccessorChange Node (Maybe Node) -- ^ The successor for some node changed to some new one
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
runArvyLocal
  :: forall m sarr tarr r r'
  . ( Members '[ Lift m, Trace ] r
      -- r represents the input effects, the one the Arvy algorithm needs. r' represents the output effects, which are the ones the algorithm needs plus it taking inputs and sending outputs. Lift and Trace are not included in these additional effects because the algorithm might need those itself.
    , r' ~ (Input (Maybe Node) ': Output (Maybe ArvyEvent) ': r)
      -- Quantified constraint because we need a mutable array to store the state of the nodes of /any/ possible algorithm, and every algorithm can choose its own state type
    , forall s . MArray sarr s m
    , MArray tarr (Maybe Node) m )
  => NodeCount
  -> GraphWeights -- ^ The graph weights, used for giving nodes access to their local weights
  -> tarr Node (Maybe Node) -- ^ The (initial) spanning tree, which this function modifies as requests are coming in. This is to allow request generators access to it from the outside.
  -> Arvy r -- ^ The Arvy algorithm to run
  -> Sem r' ()
runArvyLocal n weights tree (Arvy inst) = runArvyLocal' inst where
  runArvyLocal' :: forall msg s . ArvyInst msg s r -> Sem r' ()
  runArvyLocal' ArvyInst { .. } = do
    states <- traverse initiateState [0 .. n - 1]
    stateArray <- sendM $ newListArray (0, n - 1) states
    go stateArray
    
    where

    initiateState :: Node -> Sem r' s
    initiateState i = raise . raise $ runLocalWeights weights i (arvyNodeInit i)

    -- | The main loop which repeatedly takes a request as input and processes it on the current state
    go :: sarr Node s -> Sem r' ()
    go state = input >>= \case
      Nothing -> output Nothing
      Just i -> do
        output $ Just $ RequestMade i
        getSuccessor i >>= \case
          -- If the node that made the request has no successor, immediately grant
          Nothing -> output $ Just $ RequestGranted i i Local
          Just successor -> do
            msg <- runNode i (arvyInitiate i)
            setSuccessor i Nothing
            output $ Just $ RequestTravel i successor (show msg)
            root <- send msg successor
            output $ Just $ RequestGranted i root Received
        go state
        
      where

      -- | Send a message to some node and repeatedly applies the arvy algorithm until eventually the root node is found, which gets returned
      send :: msg Node -> Node -> Sem r' Int
      send msg i = getSuccessor i >>= \case
        Just successor -> do
          (newSucc, newMsg) <- runNode i (arvyTransmit i msg)
          setSuccessor i (Just newSucc)
          output $ Just $ RequestTravel i successor (show newMsg)
          send newMsg successor
        Nothing -> do
          newSucc <- runNode i (arvyReceive i msg)
          setSuccessor i (Just newSucc)
          return i

      -- | Convenience function for getting the successor to a node by reading from the tree array
      getSuccessor :: Node -> Sem r' (Maybe Node)
      getSuccessor i = sendM $ readArray tree i

      -- | Convenience function for setting the successor to a node by writing to the tree array
      setSuccessor :: Node -> Maybe Node -> Sem r' ()
      setSuccessor i newSucc = do
        sendM $ writeArray tree i newSucc
        output $ Just $ SuccessorChange i newSucc

      -- | Interprets the state in a node as array operations to the state array at that nodes index
      runNodeState :: Node -> Sem (State s ': r) a -> Sem r' a
      runNodeState i = raise . reinterpret \case
        Get -> sendM $ readArray state i
        Put s -> do
          sendM $ writeArray state i s
          output $ Just $ StateChange i (show s)

      -- | Runs a node with its local effects
      runNode :: Node -> Sem (LocalWeights ': State s ': r) a -> Sem r' a
      runNode i action = runNodeState i (runLocalWeights weights i action)
