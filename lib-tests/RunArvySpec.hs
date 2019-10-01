module RunArvySpec where
import           Arvy.Algorithm
import           Arvy.Local
import           Arvy.Log
import           Conduit
import qualified Data.Conduit.Combinators  as C
import           Test.Hspec
import Polysemy
import Polysemy.RandomFu
import Data.Random.List
import Data.Random.Distribution.Uniform
import Control.Monad (replicateM)
import Polysemy.State
import Polysemy.Output


runArvySpec :: Spec
runArvySpec = describe "Arvy.Local.runArvyLocal" $ do
  it "doesn't do anything for no requests" $
    runM (runArvy (zeroRoot 1) undefinedArvy []) `shouldReturn` []

  it "doesn't do much for requests from the root" $
    runM (runArvy (zeroRoot 1) undefinedArvy [0, 0, 0]) `shouldReturn` [[], [], []]

  it "doesn't change the tree for requests right next to the root" $ do
    let n = 10
    requests <- randomWalk (0, n - 1) 1000 0
    let expected = map (:[]) (0 : init requests)
    runM (runArvy (ringTree n ()) randomArvy requests) `shouldReturn` expected

  it "always finds the root" $ do
    let n = 100
    requests <- randomRequests (0, n - 1) 1000
    result <- runM $ runArvy (ringTree n ()) randomArvy requests
    result `shouldSatisfy` const True

  it "changes the tree correctly" $ do
    -- We use an algorithm that always selects the node that made the request as the new successor (ivy). Then we give a request sequence that is supposed to restore the original tree and repeat it a couple times, verifying that the requests always took the expected path
    let n = 100
        requests = n - 1 : [0..n - 2] ++ [n - 3, n - 4..0]
        expected = [n - 2, n - 3..0]
          : [n - 1]
          : [ [n - 1, s] | s <- [0..n - 3] ]
          ++ [ [s] | s <- [n - 2, n - 3..1] ]
    runM (runArvy (ringTree n ()) rootArvy (requests ++ requests ++ requests))
      `shouldReturn` (expected ++ expected ++ expected)

  it "tracks state" $ do
    let n = 100
    requests <- randomRequests (0, n - 1) 1000
    (outputs, result) <- runM $ runOutputAsList $ runArvy (ringTree n 0) randomCounterArvy requests
    length outputs `shouldBe` length (concat result)

randomRequests :: (Node, Node) -> Int -> IO [Node]
randomRequests (lower, upper) count = runM . runRandomIO $ replicateM count go where
  go :: Member RandomFu r => Sem r Node
  go = sampleRVar (uniform lower upper)

undefinedArvy :: forall r a . ArvyAlgorithm (ArvyData a) a r
undefinedArvy = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r @[] ArvyBehavior
    { arvyMakeRequest = undefined
    , arvyForwardRequest = undefined
    , arvyReceiveRequest = undefined
    }
  , arvyRunner = const raise
  }

randomWalk :: (Node, Node) -> Int -> Node -> IO [Node]
randomWalk (lower, upper) c = runM . runRandomIO . go c where
  go :: Member RandomFu r => Int -> Node -> Sem r [Node]
  go 0 _ = return []
  go count start = do
    next <- getNext start
    rest <- go (count - 1) next
    return $ next : rest

  getNext :: Member RandomFu r => Node -> Sem r Node
  getNext start
    | lower == start = return (start + 1)
    | start == upper = return (start - 1)
    | otherwise = do
        b <- sampleRVar stdUniform
        return $ if b then start - 1 else start + 1

randomArvy :: forall r a . Member RandomFu r => ArvyAlgorithm (ArvyData a) a r
randomArvy = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return [i]
    , arvyForwardRequest = \prev i _ -> do
        newSucc <- sampleRVar (randomElement prev)
        return (newSucc, i : prev)
    , arvyReceiveRequest = \prev _ -> sampleRVar (randomElement prev)
    }
  , arvyRunner = const raise
  }

randomCounterArvy :: forall r . Members '[RandomFu, Output ()] r => ArvyAlgorithm (ArvyData Int) Int r
randomCounterArvy = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @(State Int ': r) ArvyBehavior
    { arvyMakeRequest = \i _ -> return [i]
    , arvyForwardRequest = \prev i _ -> do
        modify (+1)
        newSucc <- sampleRVar (randomElement prev)
        return (newSucc, i : prev)
    , arvyReceiveRequest = \prev _ -> do
        modify (+1)
        sampleRVar (randomElement prev)
    }
  , arvyRunner = const $ reinterpret $ \case
      Get -> get
      Put v -> output () *> put v
  }

rootArvy :: forall r a . ArvyAlgorithm (ArvyData a) a r
rootArvy = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return (Identity i)
    , arvyForwardRequest = \msg@(Identity root) _ _ -> return (root, msg)
    , arvyReceiveRequest = \(Identity root) _ -> return root
    }
  , arvyRunner = const raise
  }

ringTree :: NodeCount -> a -> ArvyData a
ringTree n value = ArvyData
  { arvyDataNodeCount = n
  , arvyDataNodeData = \node -> ArvyNodeData
    { arvyNodeDataSuccessor = if node == 0 then 0 else node - 1
    , arvyNodeDataAdditional = value
    }
  }

zeroRoot :: NodeCount -> ArvyData ()
zeroRoot n = ArvyData
  { arvyDataNodeCount = n
  , arvyDataNodeData = const ArvyNodeData
    { arvyNodeDataSuccessor = 0
    , arvyNodeDataAdditional = ()
    }
  }

runArvy :: forall p a r . Member (Lift IO) r => p -> ArvyAlgorithm p a (RandomFu ': Log ': r) -> [Node] -> Sem r [[Node]]
runArvy param alg requests = runIgnoringLog . runRandomIO . runConduit
  $ yieldMany requests .| runArvyLocal @[Node] param alg .| C.sinkList
