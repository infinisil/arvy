{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE AllowAmbiguousTypes                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Algorithm where

import           Polysemy
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Trace
import           Tree

{- |
An Arvy algorithm, a combination between Arrow and Ivy.
@r@ stands for the effects it runs in, chosen by the caller, but the algorithm can enforce certain constraints on it, like requiring randomness.
@msg@ stands for the message type it sends between nodes, this can be arbitrarily chosen by the algorithm.
@i@ stands for the node index type, which can be passed via messages and be has to be used to select which node to choose as the next successor.

- When a node @x@ initiates a request, 'arvyInitiate' is called to generate the initial message, which has access to the current node's index through a 'Reader i' effect.
- This message gets passed along the path from @x@ to the current token holder @y@. For nodes the request travels *through*, 'arvyTransmit' is called, which can transform the message, potentially adding a reference to the current node's index.
- Whenever a message arrives at a node, 'arvySelect' is called, which has to select a node index from the received message. For this selection, the algorithm does *not* have access to the current node's index. This is to ensure correctness of it, because only *previously* traversed nodes should be selected.

-}
data Arvy r = forall msg n . Arvy
  { arvyNodeInit :: forall i . Sem (Reader i ': r) n
  , arvyInitiate :: forall i . Sem (State n ': Reader i ': r) (msg i)
  , arvyTransmit :: forall i . msg i -> Sem (State n ': Reader i ': r) (msg i)
  , arvySelect   :: forall i . msg i -> Sem (State n ': r) i
  }

runArvyLocal :: forall i r . Members '[Trace, SpanningTree i, Output (i, i)] r => Arvy r -> i -> Sem r ()
runArvyLocal (Arvy n s t rr) = runArvyLocal' (n, s, t, rr) where
  runArvyLocal' :: forall i r msg n
    . Members '[Trace, SpanningTree i, Output (i, i)] r
    => ( Sem (Reader i ': r) n
       , Sem (State n ': Reader i ': r) (msg i)
       , msg i -> Sem (State n ': Reader i ': r) (msg i)
       , msg i -> Sem (State n ': r) i
       )
    -> i
    -> Sem r ()
  runArvyLocal' (nodeInit, send, transfer, select) r = do
    trace "Initiating request"
    getSuccessor r >>= \case
      Nothing -> trace "Token is already here\n"
      Just n -> do
        setSuccessor r Nothing
        (_, msg) <- runReader r (runState undefined send)
        output (r, n)
        go msg n
    where
      go :: msg i -> i -> Sem r ()
      go msg r = do
        (_, nextSucc) <- runState undefined $ select msg
        currentSucc <- getSuccessor r
        setSuccessor r (Just nextSucc)
        case currentSucc of
          Nothing ->
            trace "Request arrived at token holder\n"
          Just n -> do
            (_, newMsg) <- runReader r $ runState undefined $ transfer msg
            output (r, n)
            go newMsg n

newtype ArrowMessage i = ArrowMessage i

arrow :: Arvy r
arrow = Arvy
  { arvyNodeInit = return ()
  , arvyInitiate =
      ArrowMessage <$> ask
  , arvyTransmit = \_ ->
      ArrowMessage <$> ask
  , arvySelect = \(ArrowMessage sender) ->
      return sender
  }

newtype IvyMessage i = IvyMessage i

ivy :: Arvy r
ivy = Arvy
  { arvyNodeInit = return ()
  , arvyInitiate =
      IvyMessage <$> ask
  , arvyTransmit =
      return
  , arvySelect = \(IvyMessage root) ->
      return root
  }

-- TODO: Use mono-traversable for speedup
fromSelection :: (forall i . [i] -> Sem (State () ': r) i) -> Arvy r
fromSelection select = Arvy
  { arvyNodeInit = return ()
  , arvyInitiate = do
      i <- ask
      return [i]
  , arvyTransmit = \msg -> do
      i <- ask
      return $ i : msg
  , arvySelect = select
  }

arrow' :: Arvy r
arrow' = fromSelection (return . head)

ivy' :: Arvy r
ivy' = fromSelection (return . last)

