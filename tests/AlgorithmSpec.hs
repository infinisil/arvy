module AlgorithmSpec where

import           Arvy.Algorithm
import           Arvy.Pure
import           Test.Hspec
import Polysemy
import qualified Parameters.Weights as Weights
import Arvy.Local
import qualified Data.NonNull as NN

nodeCount :: NodeCount
nodeCount = 5

weights :: GraphWeights
weights = run $ Weights.weightsGet Weights.ring nodeCount

tree :: [Node]
tree = 0 : [0 .. nodeCount - 1]

requests :: [Node]
requests = [2, 4, 1, 0, 3]

state :: [Bool]
state = replicate 5 False

events :: ([Node] -> String) -> [ArvyEvent]
events wrapper =
  [ RequestMade 2
  , SuccessorChange 2 2
  , RequestTravel 2 1 (wrapper [2])
  , SuccessorChange 1 2
  , RequestTravel 1 0 (wrapper [1,2])
  , SuccessorChange 0 2
  , RequestGranted 2 0 Received

  , RequestMade 4
  , SuccessorChange 4 4
  , RequestTravel 4 3 (wrapper [4])
  , SuccessorChange 3 4
  , RequestTravel 3 2 (wrapper [3,4])
  , SuccessorChange 2 4
  , RequestGranted 4 2 Received

  , RequestMade 1
  , SuccessorChange 1 1
  , RequestTravel 1 2 (wrapper [1])
  , SuccessorChange 2 1
  , RequestTravel 2 4 (wrapper [2,1])
  , SuccessorChange 4 1
  , RequestGranted 1 4 Received

  , RequestMade 0
  , SuccessorChange 0 0
  , RequestTravel 0 2 (wrapper [0])
  , SuccessorChange 2 0
  , RequestTravel 2 1 (wrapper [2,0])
  , SuccessorChange 1 2
  , RequestGranted 0 1 Received

  , RequestMade 3
  , SuccessorChange 3 3
  , RequestTravel 3 4 (wrapper [3])
  , SuccessorChange 4 3
  , RequestTravel 4 1 (wrapper [4,3])
  , SuccessorChange 1 3
  , RequestTravel 1 2 (wrapper [1,4,3])
  , SuccessorChange 2 1
  , RequestTravel 2 0 (wrapper [2,1,4,3])
  , SuccessorChange 0 2
  , RequestGranted 3 0 Received
  ]

-- | A simple stateful Arvy algorithm that either does Arrow (when state of current node is True) or Ivy (False), which flips every time
algorithm :: Arvy Bool r
algorithm = arvy @[] ArvyInst
  { arvyInitiate = \i _ -> return [i]
  , arvyTransmit = \msg i _ -> do
      selection <- select msg
      return (selection, i : map forward msg )
  , arvyReceive = \msg _ -> select msg
  } where
  select :: Member (State Bool) r => [i] -> Sem r i
  select msg = do
    takeHead <- get
    put (not takeHead)
    return $ if takeHead
      then head msg
      else last msg

simpleAlgorithm :: Arvy Bool r
simpleAlgorithm = simpleArvy
  (\s -> do
      takeHead <- get
      put (not takeHead)
      return $ if takeHead
        then NN.head s
        else NN.last s
  )


algorithmSpec :: Spec
algorithmSpec = do
  describe "Arvy.Local.runArvyLocal" $
    it "Returns the correct event sequence" $
      runArvyLocalPure weights tree state algorithm requests `shouldBe` events show

  describe "Arvy.Algorithm.simpleArvy" $ do
    it "Returns the same as the non-simple version" $
      runArvyLocalPure weights tree state simpleAlgorithm requests `shouldBe`
        events (\list -> "SimpleMsg (NonNull {toNullable = fromList " ++ show list ++ "})")
