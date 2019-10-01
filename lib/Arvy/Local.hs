{-# LANGUAGE OverloadedStrings #-}

module Arvy.Local
  ( runArvyLocal
  , runArvyLocal'
  , LocalRunState
  ) where

import Arvy.Algorithm
import Arvy.Log
import Polysemy
import Polysemy.State
import Data.Array.IO
import Conduit
import Data.MonoTraversable
import Data.Sequences
import qualified Data.Conduit.Combinators as C

type LocalRunState a = (IOUArray Node Node, IOArray Node a)

extractArvyDataArrays :: ArvyData a -> IO (LocalRunState a)
extractArvyDataArrays ArvyData { .. } = do
  let nodeRange = (0, arvyDataNodeCount - 1)
      (successors, additionals) = unzip
        [ (s, a)
        | node <- range nodeRange
        , let ArvyNodeData s a = arvyDataNodeData node ]
  tree <- newListArray nodeRange successors
  states <- newListArray nodeRange additionals
  return (tree, states)

-- | Run an algorithm, accepting a sequence of requests, outputting the path it took
runArvyLocal
  :: forall seq p a r
   . ( Member (Lift IO) r
     , LogMember r
     , Element seq ~ Node
     , Monoid seq
     , SemiSequence seq )
  => p -> ArvyAlgorithm p a r -> ConduitT Node seq (Sem r) ()
runArvyLocal param alg = do
  (_, conduit) <- lift $ runArvyLocal' param alg
  conduit

-- | Run an algorithm, accepting a sequence of requests, outputting the path it took
runArvyLocal'
  :: forall seq p a r
   . ( Member (Lift IO) r
     , LogMember r
     , Element seq ~ Node
     , Monoid seq
     , SemiSequence seq )
  => p -> ArvyAlgorithm p a r -> Sem r (LocalRunState a, ConduitT Node seq (Sem r) ())
runArvyLocal' param (GeneralArvy (ArvySpec behavior runner)) = runArvySpecLocal param behavior runner
runArvyLocal' param (SpecializedArvy generator (ArvySpec behavior runner)) = do
  arvyData <- generator param
  runArvySpecLocal arvyData behavior runner

runArvySpecLocal
  :: forall seq a msg r r'
   . ( Member (Lift IO) r
     , Show (msg Node)
     , LogMember r
     , Element seq ~ Node
     , SemiSequence seq
     , Monoid seq )
  => ArvyData a
  -> ArvyBehavior Node msg r'
  -> (forall x . Node -> Sem r' x -> Sem (State a ': r) x)
  -> Sem r (LocalRunState a, ConduitT Node seq (Sem r) ())
runArvySpecLocal dat ArvyBehavior { .. } runner = do
  (tree, states) <- sendM $ extractArvyDataArrays dat

  let request :: Node -> Sem r seq
      request node = do
        lgDebug $ "Request made by " <> tshow node
        successor <- getSuccessor node
        if successor == node
        then do
          lgDebug $ "Root was here all along" <> tshow node
          return mempty
        else do
          setSuccessor node node
          msg <- runNode node $ arvyMakeRequest node successor
          send msg successor

      send :: msg Node -> Node -> Sem r seq
      send msg node = do
        lgDebug $ "Node " <> tshow node <> " received message " <> tshow msg
        successor <- getSuccessor node
        if successor == node
        then do
          newSucc <- runNode node $ arvyReceiveRequest msg node
          setSuccessor node newSucc
          lgDebug $ "Root found at " <> tshow node
          return (cons node mempty)
        else do
          (newSucc, newMsg) <- runNode node $ arvyForwardRequest msg node successor
          setSuccessor node newSucc
          res <- send newMsg successor
          return $ cons node res

      runNode :: forall x . Node -> Sem r' x -> Sem r x
      runNode node sem = interpret (\case
          Get -> sendM $ readArray states node
          Put v -> sendM $ writeArray states node v
        ) (runner node sem)

      setSuccessor :: Node -> Node -> Sem r ()
      setSuccessor node newSucc = do
        lgDebug $ "Succ change: " <> tshow node <> " -> " <> tshow newSucc
        sendM $ writeArray tree node newSucc

      getSuccessor :: Node -> Sem r Node
      getSuccessor node = sendM $ readArray tree node

  return ((tree, states), C.mapM request)
