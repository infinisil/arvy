{-# LANGUAGE OverloadedStrings #-}

module Arvy.Local
  ( runArvyLocal
  ) where

import Arvy.Algorithm
import Arvy.Log
import Polysemy
import Polysemy.State
import Data.Array.IO
import Conduit
import Data.NonNull
import Data.MonoTraversable
import Data.Sequences
import qualified Data.Conduit.Combinators as C


extractArvyDataArrays :: ArvyData a -> IO (IOUArray Node Node, IOArray Node a)
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
     , MonoPointed seq
     , SemiSequence seq )
  => p -> ArvyAlgorithm p a r -> ConduitT Node (NonNull seq) (Sem r) ()
runArvyLocal param (Arrow (StaticArvySpec behavior runner)) = runArvySpecLocal param (fromStatic behavior) runner
runArvyLocal param (GeneralArvy (ArvySpec behavior runner)) = runArvySpecLocal param behavior runner
runArvyLocal param (SpecializedArvy generator (ArvySpec behavior runner)) = do
  arvyData <- lift $ generator param
  runArvySpecLocal arvyData behavior runner

runArvySpecLocal
  :: forall seq a msg r r'
   . ( Member (Lift IO) r
     , Show (msg Node)
     , LogMember r
     , Element seq ~ Node
     , SemiSequence seq
     , MonoPointed seq )
  => ArvyData a
  -> ArvyBehavior Node msg r'
  -> (forall x . Node -> Sem r' x -> Sem (State a ': r) x)
  -> ConduitT Node (NonNull seq) (Sem r) ()
runArvySpecLocal dat ArvyBehavior { .. } runner = do
  (tree, states) <- lift $ sendM $ extractArvyDataArrays dat

  let request :: Node -> Sem r (NonNull seq)
      request node = do
        lgDebug $ "Request made by " <> tshow node
        successor <- getSuccessor node
        if successor == node
        then rootFound node
        else do
          setSuccessor node node
          msg <- runNode node $ arvyMakeRequest node successor
          res <- send msg successor
          return $ cons node res

      send :: msg Node -> Node -> Sem r (NonNull seq)
      send msg node = do
        lgDebug $ "Node " <> tshow node <> " received message " <> tshow msg
        successor <- getSuccessor node
        if successor == node
        then do
          newSucc <- runNode node $ arvyReceiveRequest msg node
          setSuccessor node newSucc
          rootFound node
        else do
          (newSucc, newMsg) <- runNode node $ arvyForwardRequest msg node successor
          setSuccessor node newSucc
          res <- send newMsg successor
          return $ cons node res

      rootFound :: Node -> Sem r (NonNull seq)
      rootFound node = do
        lgDebug $ "Root found at " <> tshow node
        return (opoint node)

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

  C.mapM request
