{-# LANGUAGE FlexibleInstances #-}
module RunArvySpec where
import           Arvy.Algorithm
import           Arvy.Local
import           Arvy.Log
import           Conduit
import           Control.Monad                    (replicateM)
import qualified Data.Conduit.Combinators         as C
import           Data.Random.Distribution.Uniform
import           Data.Random.List
import           Polysemy
import           Polysemy.Output
import           Polysemy.RandomFu
import           Polysemy.State
import           Test.Hspec


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
    runM (runArvy (ringTree n) randomArvy requests) `shouldReturn` expected

  it "always finds the root" $ do
    let n = 100
    requests <- randomRequests (0, n - 1) 1000
    result <- runM $ runArvy (ringTree n) randomArvy requests
    result `shouldSatisfy` const True

  it "changes the tree correctly" $ do
    -- We use an algorithm that always selects the node that made the request as the new successor (ivy). Then we give a request sequence that is supposed to restore the original tree and repeat it a couple times, verifying that the requests always took the expected path
    let n = 100
        requests = n - 1 : [0..n - 2] ++ [n - 3, n - 4..0]
        expected = [n - 2, n - 3..0]
          : [n - 1]
          : [ [n - 1, s] | s <- [0..n - 3] ]
          ++ [ [s] | s <- [n - 2, n - 3..1] ]
    runM (runArvy (ringTree n) rootArvy (requests ++ requests ++ requests))
      `shouldReturn` (expected ++ expected ++ expected)

  it "tracks state" $ do
    let n = 100
    requests <- randomRequests (0, n - 1) 1000
    (outputs, result) <- runM $ runOutputAsList $ runArvy (ringTree n) randomCounterArvy requests
    length outputs `shouldBe` length (concat result)

randomRequests :: (Node, Node) -> Int -> IO [Node]
randomRequests (lower, upper) count = runM . runRandomIO $ replicateM count go where
  go :: Member RandomFu r => Sem r Node
  go = sampleRVar (uniform lower upper)

undefinedArvy :: forall r . GeneralArvy r
undefinedArvy = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r @[] ArvyBehavior
    { arvyMakeRequest = undefined
    , arvyForwardRequest = undefined
    , arvyReceiveRequest = undefined
    }
  , arvyInitState = \_ _ -> return ()
  , arvyRunner = const raise
  }

-- | Returns a random walk in a sequential range of values
randomWalk
  :: (Node, Node) -- ^ The lower and upper limits
  -> Int -- ^ The number of steps
  -> Node -- ^ The starting value
  -> IO [Node]
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

-- | A completely random arvy algorithm
randomArvy :: forall r . Member RandomFu r => GeneralArvy r
randomArvy = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return [i]
    , arvyForwardRequest = \prev i _ -> do
        newSucc <- sampleRVar (randomElement prev)
        return (newSucc, i : fmap forward prev)
    , arvyReceiveRequest = \prev _ -> sampleRVar (randomElement prev)
    }
  , arvyInitState = \_ _ -> return ()
  , arvyRunner = const raise
  }

-- | A completely random arvy algorithm, but one which outputs () every time a parent is selected
randomCounterArvy :: forall r . Members '[RandomFu, Output ()] r => GeneralArvy r
randomCounterArvy = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @(State Int ': r) ArvyBehavior
    { arvyMakeRequest = \i _ -> return [i]
    , arvyForwardRequest = \prev i _ -> do
        modify (+1)
        newSucc <- sampleRVar (randomElement prev)
        return (newSucc, i : fmap forward prev)
    , arvyReceiveRequest = \prev _ -> do
        modify (+1)
        sampleRVar (randomElement prev)
    }
  , arvyInitState = \_ _ -> return (0 :: Int)
  , arvyRunner = const $ reinterpret $ \case
      Get -> get
      Put v -> output () *> put v
  }

-- | An arvy algorithm that always chooses the original requesting node, aka Ivy
rootArvy :: forall r . GeneralArvy r
rootArvy = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return (Identity i)
    , arvyForwardRequest = \msg@(Identity root) _ _ -> return (root, fmap forward msg)
    , arvyReceiveRequest = \(Identity root) _ -> return root
    }
  , arvyInitState = \_ _ -> return ()
  , arvyRunner = const raise
  }

-- | A tree consisting of a certain number of nodes arranged in a ring, all of them pointing leftwards
ringTree :: NodeCount -> ArvyData ()
ringTree n = ArvyData
  { arvyDataNodeCount = n
  , arvyDataNodeData = \node -> ArvyNodeData
    { arvyNodeSuccessor = if node == 0 then 0 else node - 1
    , arvyNodeAdditional = ()
    , arvyNodeWeights = const 0
    }
  }

-- | A tree where node 0 is every node's parent
zeroRoot :: NodeCount -> ArvyData ()
zeroRoot n = ArvyData
  { arvyDataNodeCount = n
  , arvyDataNodeData = const ArvyNodeData
    { arvyNodeSuccessor = 0
    , arvyNodeWeights = const 0
    , arvyNodeAdditional = ()
    }
  }

runArvy :: forall r . (Member (Lift IO) r) => ArvyData () -> GeneralArvy (RandomFu ': Log ': r) -> [Node] -> Sem r [[Node]]
runArvy param (GeneralArvy spec) requests = runIgnoringLog $ runRandomIO $ do
  request <- runArvySpecLocal param spec
  runConduit $ yieldMany requests .| C.mapM request .| C.sinkList
