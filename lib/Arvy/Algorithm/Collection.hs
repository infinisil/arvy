{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Arvy.Algorithm.Collection
  ( arrow
  , ivy
  , ring
  , minWeight
  ) where

import           Arvy.Algorithm
import           Arvy.Weight
import           Polysemy
import           Polysemy.State
import Data.Foldable
import Data.Ord (comparing)
import qualified Data.Sequence as S
import Data.Ratio

newtype ArrowMessage i = ArrowMessage i deriving Show

arrow :: forall r a . HasState a () => GeneralArvy a r
arrow = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return (ArrowMessage i)
    , arvyForwardRequest = \(ArrowMessage sender) i _ -> return (sender, ArrowMessage i)
    , arvyReceiveRequest = \(ArrowMessage sender) _ -> return sender
    }
  , arvyRunner = \_ _ -> raise
  }


minWeight
  :: forall r a
   . ( HasWeights a
     , HasState a () )
  => GeneralArvy a r
minWeight = GeneralArvy spec where
  spec :: forall i . ArvySpec a i r
  spec = ArvySpec
    { arvyBehavior = behaviorType @(Weights i ': r) ArvyBehavior
      { arvyMakeRequest = \i _ -> return [i]
      , arvyForwardRequest = \prevs i _ -> do
          weights <- traverse weightTo prevs
          let best = fst $ minimumBy (comparing snd) (zip prevs weights)
          return (best, i : prevs)
      , arvyReceiveRequest = \prevs _ -> do
          weights <- traverse weightTo prevs
          return $ fst $ minimumBy (comparing snd) (zip prevs weights)
      }
    , arvyRunner = \_ a -> reinterpret (weightHandler (getWeights a))
    }

newtype IvyMessage i = IvyMessage i deriving Show

-- | The Ivy Arvy algorithm, which always points all nodes back to the root node where the request originated from.
ivy :: forall r a . HasState a () => GeneralArvy a r
ivy = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return (IvyMessage i)
    , arvyForwardRequest = \msg@(IvyMessage root) _ _ -> return (root, msg)
    , arvyReceiveRequest = \(IvyMessage root) _ -> return root
    }
  , arvyRunner = \_ _ -> raise
  }


data RingMessage i
  = BeforeCrossing
      { root   :: i
      , sender :: i
      }
  | Crossing
      { root   :: i
      }
  | AfterCrossing
      { sender :: i
      }
  deriving Show

data RingNodeState
  = SemiNode
  | BridgeNode
  deriving Show

data RingArvyData = RingArvyData !Node !RingNodeState

instance HasState RingArvyData RingNodeState where
  getState (RingArvyData _ s) = s

ring :: forall r . SpecializedArvy NodeCount RingArvyData r
ring = SpecializedArvy generator spec where
  generator :: NodeCount -> Sem r (ArvyData RingArvyData)
  generator n = return ArvyData
    { arvyDataNodeCount = n
    , arvyDataNodeData = \node -> RingArvyData
      ( case node `compare` root of
          LT -> node + 1
          EQ -> node
          GT -> node - 1
      ) (if node == root - 1
        then BridgeNode
        else SemiNode
      )
    } where root = n `div` 2
  spec :: ArvySpec RingArvyData i r
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
    , arvyRunner = \_ _ -> id
    }

data InbetweenMessage i = InbetweenMessage Int i (S.Seq i) deriving (Functor, Show)


inbetween :: forall r a . HasState a () => Ratio Int -> GeneralArvy a r
inbetween ratio = GeneralArvy ArvySpec
  { arvyBehavior = behaviorType @r ArvyBehavior
    { arvyMakeRequest = \i _ -> return (InbetweenMessage 1 i S.empty)
    , arvyForwardRequest = \(InbetweenMessage k f (fmap forward -> seq')) i _ ->
      let s = S.length seq' + 1
          newK = k + 1
          (newF, newSeq) = if (newK - s) % newK < ratio
            then case S.viewl seq' of
              S.EmptyL -> (i, S.empty)
              fir S.:< rest -> (fir, rest S.|> i)
            else (forward f, seq' S.|> i)
      in return (f, InbetweenMessage newK newF newSeq)
    , arvyReceiveRequest = \(InbetweenMessage _ f _) _ -> return f
    }
  , arvyRunner = \_ _ -> raise
  }

--newtype WeightedInbetweenMessage i = WeightedInbetweenMessage (NonNull [(i, Double)]) deriving Show
--
---- | @'inbetweenWeighted' ratio@ Chooses the node that lies at @ratio@ inbetween the root node and the last node by weight,
---- where 0.0 means always choose the root node, 1.0 means always choose the last node
---- This is equivalent to 'inbetween' if run on a clique
--inbetweenWeighted :: forall s r . Show s => Double -> Arvy s r
--inbetweenWeighted ratio = arvy @WeightedInbetweenMessage @s ArvyInst
--  { arvyInitiate = \i _ -> return (WeightedInbetweenMessage (opoint (i, 0)))
--  , arvyTransmit = \msg@(WeightedInbetweenMessage ps@(head -> (comingFrom, total))) i _ -> do
--      newSucc <- select msg
--      -- The newTotal is the previous total plus the weight to the node we're coming from
--      newTotal <- (total+) <$> weightTo comingFrom
--      return (newSucc, WeightedInbetweenMessage ((i, newTotal) <| mapNonNull (first forward) ps))
--  , arvyReceive = \msg _ -> select msg
--  } where
--  select :: ArvySelector WeightedInbetweenMessage s r
--  select (WeightedInbetweenMessage ps@(head -> (_, total))) = return newSucc where
--    -- Find the first node that's less than ratio * total away, starting from the most recent node
--    -- Nothing can't happen because desired is always >= 0, and ps will always contain the 0 element at the end
--    Just (newSucc, _) = find ((<= total * ratio) . snd) ps
--
--
--random :: forall s r . (Member RandomFu r, Show s) => Arvy s r
--random = arvy @Seq @s ArvyInst
--  { arvyInitiate = \i _ -> return (S.singleton i)
--  , arvyTransmit = \s i _ -> do
--      suc <- select s
--      return (suc, fmap forward s |> i)
--  , arvyReceive = \s _ -> select s
--  } where
--  select :: ArvySelector Seq s r
--  select s = sampleRVar (randomSeq s)
--
---- | Selects a random element from a 'Seq' in /O(log n)/
--randomSeq :: Seq a -> RVar a
--randomSeq s = do
--  i <- uniformT 0 (S.length s - 1)
--  return $ S.index s i
--
--
--
--newtype UtilityFunMessage i = UtilityFunMessage (NonNull [(Int, i)]) deriving Show
--
--utilityFun :: forall s r a . (Show s, Ord a) => (Int -> Double -> a) -> Arvy s r
--utilityFun f = arvy @UtilityFunMessage ArvyInst
--  { arvyInitiate = \i _ -> return $ UtilityFunMessage (opoint (0, i))
--  , arvyTransmit = \msg@(UtilityFunMessage xs) i _ -> do
--      best <- select msg
--      let newElem = (fst (head xs) + 1, i)
--      return (best, UtilityFunMessage $ newElem <| mapNonNull (second forward) xs)
--  , arvyReceive = \msg _ -> select msg
--  } where
--  select :: ArvySelector UtilityFunMessage s r
--  select (UtilityFunMessage xs) = do
--    let (indices, ids) = unzip (otoList xs)
--    weights <- traverse weightTo ids
--    let values = zipWith3 (\p d w -> (p, f d w)) ids indices weights
--        best = fst $ minimumBy (comparing snd) values
--    return best
--
---- TODO: Special functions for utility functions `w * (1 + m * (1 - e ^ (-a * i)))` and `w * ln (i * a)`
--
--data IndexMean
--  = NoIndices
--  | IndexMean Double Int
--  deriving Show
--
--type IndexMeanState = (IndexMean, Int)
--
--initialIndexMeanState :: IndexMeanState
--initialIndexMeanState = (NoIndices, 0)
--
--logWeight :: Double -> IndexMean -> IndexMean
--logWeight w NoIndices = IndexMean w 1
--logWeight w (IndexMean x n) = IndexMean (adjustedX * adjustedI) (n + 1) where
--  n' = fromIntegral n
--  adjustedX = x ** (n' / (n' + 1))
--  adjustedI = w ** (1 / (n' + 1))
--
--getIndexScore :: IndexMean -> Maybe Double
--getIndexScore NoIndices = Nothing
--getIndexScore (IndexMean x _) = Just x
--
--data IndexMeanType
--  = HopIndexBased
--  | WeightSumBased
--  deriving Show
--
--data IndexMeanMessage i = IndexMeanMessage Double [(i, Maybe Double)] deriving Show
--
--{- |
--Algorithm that logs indices of request paths at nodes, aggregating them with the geometric mean which then influences which nodes get selected.
---}
--indexMeanScore :: Member Trace r => IndexMeanType -> (Int -> Double) -> Arvy IndexMeanState r
--indexMeanScore ty af = arvy @IndexMeanMessage ArvyInst
--  { arvyInitiate = \i s -> do
--      (indexMean, _) <- get
--      w <- edgePart s
--      return (IndexMeanMessage w (opoint (i, getIndexScore indexMean)))
--  , arvyTransmit = \msg@(IndexMeanMessage w xs) i s -> do
--      best <- select msg
--      w' <- edgePart s
--      indexMean <- gets fst
--      let newMessage = IndexMeanMessage (w + w') ((i, getIndexScore indexMean) : map (first forward) xs)
--      return (best, newMessage)
--  , arvyReceive = \msg _ -> do
--      best <- select msg
--      modify (second (+1))
--      return best
--  } where
--
--  select :: ArvySelector IndexMeanMessage IndexMeanState r
--  select (IndexMeanMessage w xs) = do
--    (oldIndexMean, k) <- get
--    let a = af k
--    let newIndexMean = logWeight w oldIndexMean
--    put (newIndexMean, k)
--    scores <- traverse (\(i, iScore) -> do
--                            weight <- weightTo i
--                            return (i, getScore a iScore weight)
--                        ) xs
--    return $ fst $ minimumBy (comparing snd) scores
--
--
--  getScore :: Double -> Maybe Double -> Double -> Double
--  getScore _ Nothing weight = weight
--  getScore a (Just iScore) weight = weight ** a * iScore ** (1 - a)
--
--  edgePart :: Member (LocalWeights i) r => i -> Sem r Double
--  edgePart = case ty of
--    HopIndexBased -> \_ -> return 1
--    WeightSumBased -> weightTo
--
--data LocalMinPairsMessage i = LocalMinPairsMessage [(i, Double)] [UArray Int Double] deriving Show
--
--localMinPairs :: (Member Trace r, Show s) => Arvy s r
--localMinPairs = arvy @LocalMinPairsMessage ArvyInst
--  { arvyInitiate = \i _ -> return (LocalMinPairsMessage [(i, 0)] [listArray (0, 0) [0]])
--  , arvyTransmit = \(LocalMinPairsMessage nodes dists) i _ -> do
--      let count = length nodes
--      --trace $ "We are at count " ++ show count
--      weights <- zipWith3 (\k (j, score) weight -> (k, j, score, weight * fromIntegral count + score)) [0 :: Int ..] nodes <$> traverse (weightTo.fst) nodes
--      --trace $ "Scores determined to be " ++ show weights
--      let (bestIndex, best, bestScore, _) = minimumBy (comparing (\(_, _, _, d) -> d)) weights
--      bestWeight <- weightTo best
--      --trace $ "Selected node at index " ++ show bestIndex ++ " with node score " ++ show bestScore ++ " and weight " ++ show bestWeight
--
--      let getDist u v
--            | u == v = 0
--            | u > v = getDist v u
--            | otherwise = dists !! v ! u
--
--      let newWeights = listArray (0, count - 1) (map (\j -> getDist j bestIndex + bestWeight) [0 :: Int ..])
--      --trace $ "New weights are " ++ show newWeights
--      let newDists = dists ++ [newWeights]
--
--      let newNodes = map (\(j, (n, s)) -> (forward n, s + newWeights ! j)) (zip [0 :: Int ..] nodes)
--      --trace $ "Updated old node scores to " ++ show newNodes
--      let newNodeScore = bestScore + fromIntegral count * bestWeight
--      let newNodes' = newNodes ++ [(i, newNodeScore)]
--      --trace $ "Added new node score " ++ show newNodeScore
--
--      --let newNodes' =
--      --let newArray = array ((0, 0), (count, count)) []
--
--      return (best, LocalMinPairsMessage newNodes' newDists)
--  , arvyReceive = \(LocalMinPairsMessage nodes _) _ -> do
--      let count = length nodes
--      --trace $ "We are at the final count " ++ show count
--      weights <- zipWith (\(j, score) weight -> (j, weight * fromIntegral count + score)) nodes <$> traverse (weightTo.fst) nodes
--      let best = fst $ minimumBy (comparing snd) weights
--      return best
--  }
