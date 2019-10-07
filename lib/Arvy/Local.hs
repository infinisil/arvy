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

type LocalRunState s = (IOUArray Node Node, IOArray Node s)

extractArvyDataArrays :: ( HasSuccessor a, HasState a s ) => ArvyData a -> IO (LocalRunState s)
extractArvyDataArrays ArvyData { .. } = do
  let nodeRange = (0, arvyDataNodeCount - 1)
      (successors, additionals) = unzip
        [ (getSuccessor a, getState a)
        | node <- range nodeRange
        , let a = arvyDataNodeData node ]
  tree <- newListArray nodeRange successors
  states <- newListArray nodeRange additionals
  return (tree, states)

runArvySpecLocal
  :: forall seq a r
   . ( Member (Lift IO) r
     , HasSuccessor a
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

runArvySpecLocal'
  :: forall seq a r
   . ( Member (Lift IO) r
     , HasSuccessor a
     , LogMember r
     , Element seq ~ Node
     , SemiSequence seq
     , Monoid seq )
  => ArvyData a
  -> ArvySpec a Node r
  -> Sem r (IOUArray Node Node, Node -> Sem r seq)
runArvySpecLocal' dat ArvySpec { .. } = do
  mutableData@(tree, _) <- sendM $ extractArvyDataArrays dat
  return (tree, go mutableData arvyBehavior arvyRunner)
  where
  go
    :: forall msg s r'
     . Show (msg Node)
    => LocalRunState s
    -> ArvyBehavior Node msg r'
    -> (forall x . Node -> a -> Sem r' x -> Sem (State s ': r) x)
    -> (Node -> Sem r seq)
  go (tree, states) ArvyBehavior { .. } runner = request where
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

    runNode :: forall x . Node -> Sem r' x -> Sem r x
    runNode node sem = interpret (\case
        Get -> sendM $ readArray states node
        Put v -> sendM $ writeArray states node v
      ) (runner node undefined sem)

    setSucc :: Node -> Node -> Sem r ()
    setSucc node newSucc = do
      lgDebug $ "Succ change: " <> tshow node <> " -> " <> tshow newSucc
      sendM $ writeArray tree node newSucc

    getSucc :: Node -> Sem r Node
    getSucc node = sendM $ readArray tree node
