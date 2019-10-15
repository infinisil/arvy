{-# LANGUAGE OverloadedStrings #-}

module Arvy.Local
  ( runArvySpecLocal
  , runArvySpecLocal'
  ) where

import Arvy.Algorithm
import Arvy.Log
import Polysemy
import Polysemy.State
import Data.Array.IO
import Data.MonoTraversable
import Data.Sequences
import Data.Array.Unboxed

type LocalRunState a = (IOUArray Node Node, IOArray Node a)

extractArvyDataArrays :: Member (Lift IO) r => ArvyData a -> (NodeCount -> ArvyNodeData a -> Sem r s) -> Sem r (LocalRunState s)
extractArvyDataArrays ArvyData { .. } initState = do
  let nodeRange = (0, arvyDataNodeCount - 1)
  treeArr <- sendM $ newListArray nodeRange (map (arvyNodeSuccessor . arvyDataNodeData) (range nodeRange))
  states <- traverse (initState arvyDataNodeCount . arvyDataNodeData) (range nodeRange)
  statesArr <- sendM $ newListArray nodeRange states
  return (treeArr, statesArr)

{-# INLINE runArvySpecLocal #-}
runArvySpecLocal
  :: forall seq a r
   . ( Member (Lift IO) r
     , LogMember r
     , Element seq ~ Node
     , SemiSequence seq
     , Monoid seq )
  => ArvyData a
  -> ArvySpec a Node r
  -> Sem r (Node -> Sem r seq)
runArvySpecLocal dat spec = do
  (_, conduit) <- runArvySpecLocal' dat spec
  return conduit

{-# INLINE runArvySpecLocal' #-}
runArvySpecLocal'
  :: forall seq a r
   . ( Member (Lift IO) r
     , LogMember r
     , Element seq ~ Node
     , SemiSequence seq
     , Monoid seq )
  => ArvyData a
  -> ArvySpec a Node r
  -> Sem r (IOUArray Node Node, Node -> Sem r seq)
runArvySpecLocal' dat ArvySpec { .. } = do
  mutableData@(tree, _) <- extractArvyDataArrays dat arvyInitState
  let nodeRange = (0, arvyDataNodeCount dat - 1)
  let weights = listArray nodeRange
        [ listArray nodeRange
          [ arvyNodeWeights (arvyDataNodeData dat u) v
          | v <- range nodeRange
          ]
        | u <- range nodeRange
        ]
  return (tree, go mutableData weights arvyBehavior arvyRunner)
  where
  {-# INLINE go #-}
  go
    :: forall msg s r'
     . Show (msg Node)
    => LocalRunState s
    -> Array Node (UArray Node Weight)
    -> ArvyBehavior Node msg r'
    -> (forall x . UArray Node Weight -> Sem r' x -> Sem (State s ': r) x)
    -> (Node -> Sem r seq)
  go (tree, states) weights ArvyBehavior { .. } runner = request where
    {-# INLINE request #-}
    request :: Node -> Sem r seq
    request node = do
      lgDebug $ "Request made by " <> tshow node
      successor <- getSucc node
      if successor == node
      then do
        lgDebug $ "Root was here all along" <> tshow node
        return mempty
      else do
        setSucc node node
        msg <- runNode node $ arvyMakeRequest node successor
        send msg successor

    {-# INLINE send #-}
    send :: msg Node -> Node -> Sem r seq
    send msg node = do
      lgDebug $ "Node " <> tshow node <> " received message " <> tshow msg
      successor <- getSucc node
      if successor == node
      then do
        newSucc <- runNode node $ arvyReceiveRequest msg node
        setSucc node newSucc
        lgDebug $ "Root found at " <> tshow node
        return (cons node mempty)
      else do
        (newSucc, newMsg) <- runNode node $ arvyForwardRequest msg node successor
        setSucc node newSucc
        res <- send newMsg successor
        return $ cons node res

    {-# INLINE runNode #-}
    runNode :: forall x . Node -> Sem r' x -> Sem r x
    runNode node sem = interpret (\case
        Get -> sendM $ readArray states node
        Put v -> sendM $ writeArray states node v
      ) (runner (weights ! node) sem)

    {-# INLINE setSucc #-}
    setSucc :: Node -> Node -> Sem r ()
    setSucc node newSucc = do
      lgDebug $ "Succ change: " <> tshow node <> " -> " <> tshow newSucc
      sendM $ writeArray tree node newSucc

    {-# INLINE getSucc #-}
    getSucc :: Node -> Sem r Node
    getSucc node = sendM $ readArray tree node
