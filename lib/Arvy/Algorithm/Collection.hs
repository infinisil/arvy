{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
Description : Arvy algorithm collection
Copyright   : (c) Silvan Mosberger, 2019
License     : GPL-3
Maintainer  : contact@infinisil.com
Stability   : experimental

This module contains a number of concrete Arvy algorithms
-}
module Arvy.Algorithm.Collection
  ( arrow
  , ivy
  , random
  , edgeMin
  , module Data.Ratio
  , fixedRatio
  , weightedFixedRatio
  , localMinPairs
  , reclique
  , RecliqueConf(..)
  , dynamicStar
  , RingNodeState(..)
  , ring
  , indexMeanScore
  , IndexMeanType(..)
  ) where

import           Arvy.Algorithm
import           Arvy.Log
import           Data.Array.Unboxed
import           Data.Bifunctor
import           Data.Foldable
import           Data.IntMap                      (IntMap)
import qualified Data.IntMap                      as IntMap
import           Data.Maybe                       (fromMaybe)
import           Data.MonoTraversable
import qualified Data.NonNull                     as NN
import           Data.Ord                         (comparing)
import           Data.Random.Distribution.Uniform
import           Data.Random.RVar
import           Data.Ratio
import qualified Data.Sequence                    as S
import           Polysemy
import           Polysemy.RandomFu
import           Polysemy.State

newtype ArrowMessage i = ArrowMessage i deriving Show

-- | Arrow algorithm: Always invert arrows, maintains tree structure
arrow :: forall r . GeneralArvy r
arrow = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return (ArrowMessage i)
    , arvyForwardRequest = \(ArrowMessage sender) i _ -> return (sender, ArrowMessage i)
    , arvyReceiveRequest = \(ArrowMessage sender) _ -> return sender
    }
  , arvyInitState = \_ _ -> return ()
  , arvyRunner = const raise
  }

newtype IvyMessage i = IvyMessage i deriving (Functor, Show)

-- | Ivy Arvy algorithm: Always points all nodes back to the root node where the request originated from
ivy :: forall r . GeneralArvy r
ivy = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return (IvyMessage i)
    , arvyForwardRequest = \msg@(IvyMessage root) _ _ -> return (root, fmap forward msg)
    , arvyReceiveRequest = \(IvyMessage root) _ -> return root
    }
  , arvyInitState = \_ _ -> return ()
  , arvyRunner = const raise
  }


-- | A completely random algorithm: Selecting a parent out of the available ones at random
random :: forall r . Member RandomFu r => GeneralArvy r
random = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return (S.singleton i)
    , arvyForwardRequest = \s i _ -> do
        suc <- sampleRVar (randomSeq s)
        return (suc, fmap forward s S.|> i)
    , arvyReceiveRequest = \s _ -> sampleRVar (randomSeq s)
    }
  , arvyInitState = \_ _ -> return ()
  , arvyRunner = const raise
  }

-- | Selects a random element from a 'Seq' in /O(log n)/
randomSeq :: S.Seq a -> RVar a
randomSeq s = do
  i <- uniformT 0 (S.length s - 1)
  return $ S.index s i


-- | Edge Cost Minimizer algorithm: Connects to the shortest edge possible
{-# INLINE edgeMin #-}
edgeMin :: forall r . GeneralArvy r
edgeMin = GeneralArvy spec where
  {-# INLINE spec #-}
  spec :: forall i . NodeIndex i => ArvySpec () i r
  spec = ArvySpec
    { arvyBehavior = behaviorType @(LocalWeights i ': r) ArvyBehavior
      { arvyMakeRequest = \i _ -> return [i]
      , arvyForwardRequest = \prevs i _ -> do
          weights <- traverse weightTo prevs
          let best = fst $ minimumBy (comparing snd) (zip prevs weights)
          return (best, i : map forward prevs)
      , arvyReceiveRequest = \prevs _ -> do
          weights <- traverse weightTo prevs
          return $ fst $ minimumBy (comparing snd) (zip prevs weights)
      }
    , arvyInitState = \_ _ -> return ()
    , arvyRunner = \weights -> reinterpret (weightHandler weights)
    }


data LocalMinPairsMessage i = LocalMinPairsMessage
  { totalFromNodes    :: [(i, Double)]
    -- ^ Representing the total pair distance from specific nodes to all others
  , shortestDistances :: [UArray Int Double]
    -- ^ Representing shortest distances from all nodes to all others in a really convoluted and too complicated way
  } deriving Show

-- | Local Pair Distance Minimizer heuristic: Tries to minimize total pair distance in the local tree of all nodes in the request path
localMinPairs :: forall r . LogMember r => GeneralArvy r
localMinPairs = GeneralArvy spec where
  spec :: forall i . NodeIndex i => ArvySpec () i r
  spec = ArvySpec
    { arvyBehavior = behaviorType @(LocalWeights i ': r) ArvyBehavior
      { arvyMakeRequest = \i _ -> return (LocalMinPairsMessage [(i, 0)] [listArray (0, 0) [0]])
      , arvyForwardRequest = \(LocalMinPairsMessage nodes dists) i _ -> do
          let count = length nodes
          weights <- zipWith3 (\k (j, score) weight ->
                                 -- With some calculating, the increase of total pair distance is exactly
                                 -- (the edge to that node times the number of nodes) plus
                                 -- the total pair distance from that node to all others
                                 (k, j, score, weight * fromIntegral count + score))
                     [0..] nodes <$> traverse (weightTo . fst) nodes
          -- This is now the index, node and best score of the node we want to connect to for lowest total pair distance
          let (bestIndex, best, bestScore, _) = minimumBy (comparing (\(_, _, _, d) -> d)) weights
          -- Get the weight again to that node
          bestWeight <- weightTo best

          -- Updating the values somehow
          -- TODO: Improve this, I have no idea what's going on
          let getDist u v
                | u == v = 0
                | u > v = getDist v u
                | otherwise = dists !! v ! u

              newWeights = listArray (0, count - 1) (map (\j -> getDist j bestIndex + bestWeight) [0 :: Int ..])
              newDists = dists ++ [newWeights]

              newNodes = map (\(j, (n, s)) -> (forward n, s + newWeights ! j)) (zip [0 :: Int ..] nodes)
              newNodeScore = bestScore + fromIntegral count * bestWeight
              newNodes' = newNodes ++ [(i, newNodeScore)]

          return (best, LocalMinPairsMessage newNodes' newDists)
      , arvyReceiveRequest = \(LocalMinPairsMessage nodes _) _ -> do
          let count = length nodes
          --trace $ "We are at the final count " ++ show count
          weights <- zipWith (\(j, score) weight -> (j, weight * fromIntegral count + score)) nodes <$> traverse (weightTo.fst) nodes
          let best = fst $ minimumBy (comparing snd) weights
          return best
      }
    , arvyInitState = \_ _ -> return ()
    , arvyRunner = \weights -> reinterpret (weightHandler weights)
    }


newtype FixedRatioMessage i = FixedRatioMessage (S.Seq i) deriving (Functor, Show)

-- | The Fixed Ratio algorithm: Chooses a node a fixed ratio between the request sender and the current node
fixedRatio :: forall r . Ratio Int -> GeneralArvy r
fixedRatio ratio = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return (FixedRatioMessage (S.singleton i))
    , arvyForwardRequest = \(FixedRatioMessage seq') i _ ->
      return (select seq', FixedRatioMessage (fmap forward seq' S.|> i))
    , arvyReceiveRequest = \(FixedRatioMessage seq') _ -> return (select seq')
    }
  , arvyInitState = \_ _ -> return ()
  , arvyRunner = const raise
  } where
  select :: S.Seq a -> a
  select s = s `S.index` floor (fromIntegral (S.length s - 1) * ratio)

newtype WeightedFixedRatioMessage i = WeightedFixedRatioMessage (NN.NonNull [(i, Double)]) deriving Show

-- | @'weightedFixedRatio' ratio@ Chooses the node that lies at @ratio@ inbetween the root node and the last node by weight,
-- where 0.0 means always choose the root node, 1.0 means always choose the last node
-- This is equivalent to 'fixedRatio' if run on a clique
weightedFixedRatio :: forall r . Double -> GeneralArvy r
weightedFixedRatio ratio = GeneralArvy spec where
  spec :: forall i . NodeIndex i => ArvySpec () i r
  spec = ArvySpec
    { arvyBehavior = behaviorType @(LocalWeights i ': r) ArvyBehavior
      { arvyMakeRequest = \i _ -> return (WeightedFixedRatioMessage (opoint (i, 0)))
      , arvyForwardRequest = \msg@(WeightedFixedRatioMessage ps@(NN.head -> (comingFrom, total))) i _ -> do
          newSucc <- select msg
          -- The newTotal is the previous total plus the weight to the node we're coming from
          newTotal <- (total+) <$> weightTo comingFrom
          return (newSucc, WeightedFixedRatioMessage ((i, newTotal) NN.<| NN.mapNonNull (first forward) ps))
      , arvyReceiveRequest = \msg _ -> select msg
      }
    , arvyInitState = \_ _ -> return ()
    , arvyRunner = \weights -> reinterpret (weightHandler weights)
    } where
    select :: WeightedFixedRatioMessage (Pred i) -> Sem (LocalWeights i ': r) (Pred i)
    select (WeightedFixedRatioMessage ps@(NN.head -> (_, total))) = return newSucc where
      -- Find the first node that's less than ratio * total away, starting from the most recent node
      -- Nothing can't happen because desired is always >= 0, and ps will always contain the 0 element at the end
      Just (newSucc, _) = find ((<= total * ratio) . snd) (NN.toNullable ps)



type DynamicStarState i = UArray i Int
data DynamicStarMessage i = DynamicStarMessage
  { dynStarMsgRoot      :: !i
  , dynStarMsgRootCount :: !Int
  , dynStarMsgBest      :: !i
  , dynStarMsgBestScore :: !Double
  } deriving Show

-- | Dynamic star heuristic: Measures request probabilities and decides on a best star center based on those
-- For approximating request probabilities, only the requesting node sends along its updated request count
{-# INLINE dynamicStar #-}
dynamicStar :: forall r . LogMember r => GeneralArvy r
dynamicStar = GeneralArvy spec where
  {-# INLINE spec #-}
  spec :: forall i . NodeIndex i => ArvySpec () i r
  spec = ArvySpec
    -- The node states represent request counts for all other nodes which can be transformed to probability estimates
    { arvyBehavior = behaviorType @(LocalWeights i ': State (DynamicStarState i) ': r) ArvyBehavior
      { arvyMakeRequest = \i _ -> do
          -- Increase this nodes request count
          newCount <- (+1) <$> gets (! i)
          modify (// [(i, newCount)])

          -- Send along the new request count for this node and the local score
          DynamicStarMessage i newCount i <$> getLocalScore
      , arvyForwardRequest = \DynamicStarMessage { .. } i _ -> do
          -- Update the local request probability estimates of the requesting node
          modify (// [(forward dynStarMsgRoot, dynStarMsgRootCount)])

          -- Decide whether this node would be a better star center than all previous ones
          score <- getLocalScore
          let (newBest, newBestScore) = case dynStarMsgBestScore `compare` score of
                GT -> (i, score)
                _  -> (forward dynStarMsgBest, dynStarMsgBestScore)

          -- Always select the best star center as determined by the previous message sender, but send along the new best center
          -- for the next one. O(1) message complexity!
          return ( dynStarMsgBest
                 , DynamicStarMessage (forward dynStarMsgRoot) dynStarMsgRootCount newBest newBestScore)

      , arvyReceiveRequest = \DynamicStarMessage { .. } _ -> do
          -- Update the local request probability estimates of the requesting node
          modify (// [(forward dynStarMsgRoot, dynStarMsgRootCount)])
          return dynStarMsgBest
      }
    , arvyInitState = \nodeCount _ ->
        -- All node states are initialized with all 0
        return (listArray (0, nodeCount - 1) (replicate nodeCount 0) :: UArray Node Int)
    , arvyRunner = \weights -> interpret (weightHandler weights)
    } where

    -- | Calculates the probability weighted pair distance of all pairs, as per the calculation of c_R in \"/Low complexity variants of the Arrow distributed directory/\" (Peleg and Reshef)
    {-# INLINE getLocalScore #-}
    getLocalScore :: Sem (LocalWeights i ': State (DynamicStarState i) ': r) Double
    getLocalScore = do
      arr <- get
      weights <- allWeights
      lgDebug $ "Counts: " <> tshow (elems arr)
      lgDebug $ "Weights: " <> tshow (elems weights)
      let total = sum $ elems arr
          nodeRange = bounds arr
          summands =
            [ fromIntegral (arr ! u) * fromIntegral (arr ! v) * (weights ! u + weights ! v)
            | u <- range nodeRange
            , v <- range nodeRange
            , index nodeRange u < index nodeRange v
            ]
          score = sum summands / fromIntegral total ^^ (2 :: Int)
      lgDebug $ "Score: " <> tshow score
      return score



-- | Configuration for a recursive clique
data RecliqueConf = RecliqueConf
  { recliqueFactor :: Double
  -- ^ How much the distance increases with an additional level, should be > 1
  , recliqueLevels :: Int
  -- ^ How many levels there should be
  , recliqueBase   :: Int
  -- ^ How many more nodes each level has
  } deriving (Show)

-- | How many nodes a reclique has
recliqueNodeCount :: RecliqueConf -> NodeCount
recliqueNodeCount RecliqueConf { .. } = recliqueBase ^ recliqueLevels

recliqueLayers :: RecliqueConf -> Node -> [Int]
recliqueLayers RecliqueConf { .. } = reverse . go recliqueLevels where
  go :: Int -> Int -> [Int]
  go 0 _ = []
  go k x = b : go (k - 1) a where
    (a, b) = divMod x recliqueBase

recliqueUnlayers :: RecliqueConf -> [Int] -> Node
recliqueUnlayers RecliqueConf { .. } = foldl (\acc el -> acc * recliqueBase + el) 0

-- | A recursive clique graph. This is a clique of `recliqueBase` nodes, where each node contains a clique of `recliqueBase` nodes itself, and so on, `recliqueLevels` deep. Different nodes that are in the same lowest layer have distance 1 between them. Nodes in a different lowest layer but the same second-lowest layer have distance `recliqueFactor` between them, one layer up distance `recliqueFactor ^^ 2`, and so on.
recliqueWeights :: RecliqueConf -> Node -> Node -> Weight
recliqueWeights RecliqueConf { .. } u v
  | u == v = 0
  | otherwise = recliqueFactor ^^ (dist u v - 1)
  where
    dist :: Int -> Int -> Int
    dist a b
      | a == b = 0
      | otherwise = 1 + dist (div a recliqueBase) (div b recliqueBase)

recliqueInitialState :: RecliqueConf -> Node -> Maybe Int
recliqueInitialState _ 0 = Nothing
recliqueInitialState conf@RecliqueConf { .. } node = rightmostZero $ reverse (recliqueLayers conf node) where
  rightmostZero :: [Int] -> Maybe Int
  rightmostZero []     = Nothing
  rightmostZero (0:xs) = (+1) <$> rightmostZero xs
  rightmostZero _      = Just 0

recliqueSuccessor :: RecliqueConf -> Node -> Node
recliqueSuccessor conf node = recliqueUnlayers conf newLayers where
  layers = recliqueLayers conf node
  newLayers = reverse $ zeroLeftmost $ reverse layers
  zeroLeftmost :: [Int] -> [Int]
  zeroLeftmost []     = []
  zeroLeftmost (0:xs) = 0 : zeroLeftmost xs
  zeroLeftmost (_:xs) = 0 : xs

data RecliqueMessage i = RecliqueMessage Int i (IntMap i) deriving Show

-- | Recursive clique heuristic: Specific to recursive clique graphs as described in the paper
-- Only constant state and message size
{-# INLINE reclique #-}
reclique :: forall r . SpecializedArvy RecliqueConf (Maybe Int) r
reclique = SpecializedArvy gen spec where
  gen :: RecliqueConf -> Sem r (ArvyData (Maybe Int))
  gen conf = return ArvyData
    { arvyDataNodeCount = recliqueNodeCount conf
    , arvyDataNodeData = \node -> ArvyNodeData
      { arvyNodeSuccessor = recliqueSuccessor conf node
      , arvyNodeAdditional = recliqueInitialState conf node
      , arvyNodeWeights = recliqueWeights conf node
      }
    }
  {-# INLINE spec #-}
  spec :: forall i . NodeIndex i => ArvySpec (Maybe Int) i r
  spec = ArvySpec
    { arvyBehavior = behaviorType @(State (Maybe Int) ': r) @RecliqueMessage ArvyBehavior
      { arvyMakeRequest = \i _ -> do
          thisLevel <- fromMaybe (error "Bug in the arvy runner, makeRequest called for the root") <$> get
          put Nothing
          return (RecliqueMessage thisLevel i IntMap.empty)
      , arvyForwardRequest = \(RecliqueMessage recvLevel root levelMap) i _ -> do
          let newSucc = IntMap.findWithDefault root recvLevel levelMap
          let newLevelMap = foldr (\el acc -> IntMap.insert el i acc) (fmap forward levelMap) [0..recvLevel - 1]
          thisLevel <- fromMaybe (error "Bug in the arvy runner, forwardRequest called for the root") <$> get
          put (Just recvLevel)
          return (newSucc, RecliqueMessage thisLevel (forward root) newLevelMap)
      , arvyReceiveRequest = \(RecliqueMessage recvLevel root levelMap) _ -> do
          let newSucc = IntMap.findWithDefault root recvLevel levelMap
          put (Just recvLevel)
          return newSucc
      }
    , arvyInitState = \_ -> return . arvyNodeAdditional
    , arvyRunner = const id
    }



data RingMessage i
  = BeforeCrossing
      { root   :: i
      , sender :: i
      }
    -- ^ A message sent before crossing the bridge
  | Crossing
      { root   :: i
      }
    -- ^ A message sent accross the bridge
  | AfterCrossing
      { sender :: i
      }
    -- ^ A message sent after crossing the bridge
  deriving Show

data RingNodeState
  = SemiNode -- ^ A normal node
  | BridgeNode -- ^ The node whose parent lies across the ring bridge
  deriving Show

-- | The Arvy heuristic specialized to rings, presented in \"/The Arvy Distributed Directory Protocol/\"
-- It splits the ring into two halves and maintains a bridge between them. The two halfes stay the same,
-- but the bridge is modified to point to the requesting node when traversed.
ring :: forall r . SpecializedArvy NodeCount RingNodeState r
ring = SpecializedArvy ringGenerator spec where
  -- | A function for generating an initial ring for a specific number of nodes with the needed initial states for each node
  ringGenerator :: NodeCount -> Sem r (ArvyData RingNodeState)
  ringGenerator n = return ArvyData
    { arvyDataNodeCount = n
    , arvyDataNodeData = \node -> ArvyNodeData
      { arvyNodeSuccessor = case node `compare` root of
          LT -> node + 1
          EQ -> node
          GT -> node - 1
      , arvyNodeAdditional = if node == root - 1
          then BridgeNode
          else SemiNode
      , arvyNodeWeights = \other ->
          let
            low = min node other
            mid = max node other
            high = low + n
            dist = min (mid - low) (high - mid)
          in fromIntegral dist
      }
    } where root = n `div` 2

  spec :: NodeIndex i => ArvySpec RingNodeState i r
  spec = ArvySpec
    { arvyBehavior = behaviorType @(State RingNodeState ': r) ArvyBehavior
      { arvyMakeRequest = \i _ -> get >>= \case
          -- If our initial node is part of a semi-circle, the message won't be crossing the bridge yet if at all
          SemiNode -> return (BeforeCrossing i i)
          -- If our initial node is the bridge node, the message will travel accross the bridge, and our current node will become a semi-circle one
          BridgeNode -> do
            put SemiNode
            return (Crossing i)

      , arvyForwardRequest = \msg i _ -> case msg of
          BeforeCrossing { root, sender } -> get >>= \case
            -- If we haven't crossed the bridge yet, and the message traverses through another non-bridge node
            SemiNode ->
              return (sender, BeforeCrossing (forward root) i)
            -- If however we're the bridge node, we send a crossing message and make the current node a semi-circle one
            BridgeNode -> do
              put SemiNode
              return (sender, Crossing (forward root))
          Crossing { root } -> do
            -- If we received a message saying that the bridge was just crossed, make the current node the next bridge start
            put BridgeNode
            return (root, AfterCrossing i)
          AfterCrossing { sender } ->
            return (sender, AfterCrossing i)

      , arvyReceiveRequest = \msg _ -> case msg of
          BeforeCrossing { sender } -> return sender
          Crossing { root } -> do
            put BridgeNode
            return root
          AfterCrossing { sender } -> return sender
      }
    , arvyInitState = \_ ArvyNodeData { .. } -> return arvyNodeAdditional
    , arvyRunner = const id
    }


data IndexMean
  = NoIndices
  | IndexMean Double Int
  deriving Show

type IndexMeanState = (IndexMean, Int)

initialIndexMeanState :: IndexMeanState
initialIndexMeanState = (NoIndices, 0)

logWeight :: Double -> IndexMean -> IndexMean
logWeight w NoIndices = IndexMean w 1
logWeight w (IndexMean x n) = IndexMean (adjustedX * adjustedI) (n + 1) where
  n' = fromIntegral n
  adjustedX = x ** (n' / (n' + 1))
  adjustedI = w ** (1 / (n' + 1))

getIndexScore :: IndexMean -> Maybe Double
getIndexScore NoIndices       = Nothing
getIndexScore (IndexMean x _) = Just x

data IndexMeanType
  = HopIndexBased
  | WeightSumBased
  deriving Show

data IndexMeanMessage i = IndexMeanMessage Double [(i, Maybe Double)] deriving Show

{- |
Algorithm that logs indices of request paths at nodes, aggregating them with the geometric mean which then influences which nodes get selected.
-}
indexMeanScore :: forall r . LogMember r => IndexMeanType -> (Int -> Double) -> GeneralArvy r
indexMeanScore ty af = GeneralArvy spec where
  {-# INLINE spec #-}
  spec :: forall i . NodeIndex i => ArvySpec () i r
  spec = ArvySpec
    { arvyBehavior = behaviorType @(LocalWeights (Succ i) ': State IndexMeanState ': r) ArvyBehavior
      { arvyMakeRequest = \i s -> do
          (indexMean, _) <- get
          w <- edgePart s
          return (IndexMeanMessage w (opoint (i, getIndexScore indexMean)))
      , arvyForwardRequest = \msg@(IndexMeanMessage w xs) i s -> do
          best <- select msg
          w' <- edgePart s
          indexMean <- gets fst
          let newMessage = IndexMeanMessage (w + w') ((i, getIndexScore indexMean) : map (first forward) xs)
          return (best, newMessage)
      , arvyReceiveRequest = \msg _ -> do
          best <- select msg
          modify (second (+1))
          return best
      }
    , arvyInitState = \_ _ -> return initialIndexMeanState
    , arvyRunner = \weights -> interpret (weightHandler weights)
    } where

    {-# INLINE edgePart #-}
    edgePart :: forall r' . Member (LocalWeights (Succ i)) r' => Succ i -> Sem r' Double
    edgePart = case ty of
      HopIndexBased  -> \_ -> return 1
      WeightSumBased -> weightTo


    {-# INLINE select #-}
    select :: IndexMeanMessage (Pred i) -> Sem (LocalWeights (Succ i) ': State IndexMeanState ': r) (Pred i)
    select (IndexMeanMessage w xs) = do
      (oldIndexMean, k) <- get
      let a = af k
      let newIndexMean = logWeight w oldIndexMean
      put (newIndexMean, k)
      scores <- traverse (\(i, iScore) -> do
                              weight <- weightTo i
                              return (i, getScore a iScore weight)
                          ) xs
      return $ fst $ minimumBy (comparing snd) scores


  {-# INLINE getScore #-}
  getScore :: Double -> Maybe Double -> Double -> Double
  getScore _ Nothing weight       = weight
  getScore a (Just iScore) weight = weight ** a * iScore ** (1 - a)
