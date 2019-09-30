module Arvy.Local where

import Arvy.Algorithm
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

-- | Run an algorithm, accepting a sequence of requests, outputting what happens and stuff
runArvyLocal
  :: ( Member (Lift IO) r
     , Element seq ~ Node
     , MonoPointed seq
     , SemiSequence seq )
  => p -> ArvyAlgorithm p a r -> ConduitT Node (NonNull seq) (Sem r) ()
runArvyLocal param (Arrow (StaticArvySpec behavior runner)) = runStaticArvySpecLocal param behavior runner
runArvyLocal param (GeneralArvy (ArvySpec behavior runner)) = undefined
runArvyLocal param (SpecializedArvy generator (ArvySpec behavior runner)) = undefined

type RequestPath = [Node]

runStaticArvySpecLocal
  :: forall seq a msg r r'
   . ( Member (Lift IO) r
     , Element seq ~ Node
     , SemiSequence seq
     , MonoPointed seq )
  => ArvyData a
  -> (forall i . NodeIndex i => StaticArvyBehavior i msg r')
  -> (forall x . Node -> Sem r' x -> Sem (State a ': r) x)
  -> ConduitT Node (NonNull seq) (Sem r) ()
runStaticArvySpecLocal dat@ArvyData { .. } StaticArvyBehavior { .. } runner = do
  (tree, states) <- lift $ sendM $ extractArvyDataArrays dat

  let go :: Node -> Sem r (NonNull seq)
      go node = do
        successor <- sendM $ readArray tree node
        if successor == node
        then return (opoint node)
        else do
          sendM $ writeArray tree node node
          msg <- runNode node $ staticArvyMakeRequest node successor
          res <- send node msg successor
          return $ ncons node res

      send :: Node -> msg Node -> Node -> Sem r seq
      send sender msg node = do
        successor <- sendM $ readArray tree node
        sendM $ writeArray tree node sender
        if successor == node
        then do
          runNode node $ staticArvyReceiveRequest msg node
          return (opoint node)
        else do
          newMsg <- runNode node $ staticArvyForwardRequest msg node successor
          res <- send node newMsg successor
          return $ cons node res
      runNode :: forall x . Node -> Sem r' x -> Sem r x
      runNode node sem = interpret (\case
          Get -> sendM $ readArray states node
          Put v -> sendM $ writeArray states node v
        ) (runner node sem)

  C.mapM go
