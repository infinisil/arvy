{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Arvy.Algorithm.Collection
  ( arrow
  , ivy
  , half
  , constantRing
  , inbetween
  , random
  , genArrow
  , inbetweenWeighted
  , utilityFun
  , RingNodeState(..)
  , IndexMean(..)
  , indexMeanScore
  , initialIndexMeanState
  , IndexMeanType(..)
  , localMinPairs
  ) where

import Arvy.Algorithm
import Data.Sequences
import Data.NonNull hiding (minimumBy)
import Data.Ratio
import qualified Data.Sequence as S
import Data.Sequence (Seq, (|>), ViewL(..))
import Polysemy
import Polysemy.RandomFu
import Data.Random
import Data.MonoTraversable
import Data.Ord (comparing)
import Prelude hiding (head)
import Data.Bifunctor
import Data.List (minimumBy)
import Polysemy.Trace
import Data.Array.Unboxed

genArrow :: forall s r . Show s => Arvy s r
genArrow = simpleArvy $ \xs -> do
  weights <- traverse (\i -> (i,) <$> weightTo i) (otoList xs)
  return $ fst $ minimumByEx (comparing snd) weights

newtype ArrowMessage i = ArrowMessage i deriving Show

-- | The Arrow Arvy algorithm, which always inverts all edges requests travel through. The shape of the tree therefore always stays the same
arrow :: forall s r . Show s => Arvy s r
arrow = arvy @ArrowMessage @s ArvyInst
  { arvyInitiate = \i _ -> return (ArrowMessage i)
  , arvyTransmit = \(ArrowMessage sender) i _ ->
      return (sender, ArrowMessage i)
  , arvyReceive = \(ArrowMessage sender) _ ->
      return sender
  }


newtype IvyMessage i = IvyMessage i deriving Show

-- | The Ivy Arvy algorithm, which always points all nodes back to the root node where the request originated from.
ivy :: forall s r . Show s => Arvy s r
ivy = arvy @IvyMessage @s ArvyInst
  { arvyInitiate = \i _ -> return (IvyMessage i)
  , arvyTransmit = \(IvyMessage root) _ _ ->
      return (root, IvyMessage (forward root))
  , arvyReceive = \(IvyMessage root) _ ->
      return root
  }

-- | An Arvy algorithm that always chooses the node in the middle of the traveled through path as the new successor.
half :: Show s => Arvy s r
half = simpleArvy middle where
  middle xs = return $ xs' `unsafeIndex` (lengthIndex xs' `div` 2) where
    xs' = toNullable xs

data InbetweenMessage i = InbetweenMessage Int i (Seq i) deriving (Functor, Show)

inbetween :: forall s r . Show s => Ratio Int -> Arvy s r
inbetween ratio = arvy @InbetweenMessage @s ArvyInst
  { arvyInitiate = \i _ -> return (InbetweenMessage 1 i S.empty)
  , arvyTransmit = \(InbetweenMessage k f (fmap forward -> seq')) i _ ->
      let s = S.length seq' + 1
          newK = k + 1
          (newF, newSeq) = if (newK - s) % newK < ratio
            then case S.viewl seq' of
              EmptyL -> (i, S.empty)
              fir :< rest -> (fir, rest |> i)
            else (forward f, seq' |> i)
      in return (f, InbetweenMessage newK newF newSeq)
  , arvyReceive = \(InbetweenMessage _ f _) _ -> return f
  }

newtype WeightedInbetweenMessage i = WeightedInbetweenMessage (NonNull [(i, Double)]) deriving Show

-- | @'inbetweenWeighted' ratio@ Chooses the node that lies at @ratio@ inbetween the root node and the last node by weight,
-- where 0.0 means always choose the root node, 1.0 means always choose the last node
-- This is equivalent to 'inbetween' if run on a clique
inbetweenWeighted :: forall s r . Show s => Double -> Arvy s r
inbetweenWeighted ratio = arvy @WeightedInbetweenMessage @s ArvyInst
  { arvyInitiate = \i _ -> return (WeightedInbetweenMessage (opoint (i, 0)))
  , arvyTransmit = \msg@(WeightedInbetweenMessage ps@(head -> (comingFrom, total))) i _ -> do
      newSucc <- select msg
      -- The newTotal is the previous total plus the weight to the node we're coming from
      newTotal <- (total+) <$> weightTo comingFrom
      return (newSucc, WeightedInbetweenMessage ((i, newTotal) <| mapNonNull (first forward) ps))
  , arvyReceive = \msg _ -> select msg
  } where
  select :: ArvySelector WeightedInbetweenMessage s r
  select (WeightedInbetweenMessage ps@(head -> (_, total))) = return newSucc where
    -- Find the first node that's less than ratio * total away, starting from the most recent node
    -- Nothing can't happen because desired is always >= 0, and ps will always contain the 0 element at the end
    Just (newSucc, _) = find ((<= total * ratio) . snd) ps


random :: forall s r . (Member RandomFu r, Show s) => Arvy s r
random = arvy @Seq @s ArvyInst
  { arvyInitiate = \i _ -> return (S.singleton i)
  , arvyTransmit = \s i _ -> do
      suc <- select s
      return (suc, fmap forward s |> i)
  , arvyReceive = \s _ -> select s
  } where
  select :: ArvySelector Seq s r
  select s = sampleRVar (randomSeq s)

-- | Selects a random element from a 'Seq' in /O(log n)/
randomSeq :: Seq a -> RVar a
randomSeq s = do
  i <- uniformT 0 (S.length s - 1)
  return $ S.index s i


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

-- TODO: Redesign parameters such that Arvy algorithms can specify an initial tree
-- | An Arvy algorithm that runs in constant competitive ratio on ring graphs. It works by splitting the ring into two semi-circles, connected by a bridge. The semi-circles are always kept intact, but whenever the bridge is traversed, the root node is selected as the new bridge end, while the previous bridge end becomes the new bridge start.
constantRing :: Arvy RingNodeState r
constantRing = arvy @RingMessage @RingNodeState ArvyInst
  { arvyInitiate = \i _ -> get >>= \case
      -- If our initial node is part of a semi-circle, the message won't be crossing the bridge yet if at all
      SemiNode -> return (BeforeCrossing i i)
      -- If our initial node is the bridge node, the message will travel accross the bridge, and our current node will become a semi-circle one
      BridgeNode -> do
        put SemiNode
        return (Crossing i)

  , arvyTransmit = \msg i _ -> case msg of
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

  , arvyReceive = \msg _ -> case msg of
      BeforeCrossing { sender } -> return sender
      Crossing { root } -> do
        put BridgeNode
        return root
      AfterCrossing { sender } -> return sender
  }

newtype UtilityFunMessage i = UtilityFunMessage (NonNull [(Int, i)]) deriving Show

utilityFun :: forall s r a . (Show s, Ord a) => (Int -> Double -> a) -> Arvy s r
utilityFun f = arvy @UtilityFunMessage ArvyInst
  { arvyInitiate = \i _ -> return $ UtilityFunMessage (opoint (0, i))
  , arvyTransmit = \msg@(UtilityFunMessage xs) i _ -> do
      best <- select msg
      let newElem = (fst (head xs) + 1, i)
      return (best, UtilityFunMessage $ newElem <| mapNonNull (second forward) xs)
  , arvyReceive = \msg _ -> select msg
  } where
  select :: ArvySelector UtilityFunMessage s r
  select (UtilityFunMessage xs) = do
    let (indices, ids) = unzip (otoList xs)
    weights <- traverse weightTo ids
    let values = zipWith3 (\p d w -> (p, f d w)) ids indices weights
        best = fst $ minimumBy (comparing snd) values
    return best

-- TODO: Special functions for utility functions `w * (1 + m * (1 - e ^ (-a * i)))` and `w * ln (i * a)`

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
getIndexScore NoIndices = Nothing
getIndexScore (IndexMean x _) = Just x

data IndexMeanType
  = HopIndexBased
  | WeightSumBased
  deriving Show

data IndexMeanMessage i = IndexMeanMessage Double [(i, Maybe Double)] deriving Show

{- |
Algorithm that logs indices of request paths at nodes, aggregating them with the geometric mean which then influences which nodes get selected.
-}
indexMeanScore :: Member Trace r => IndexMeanType -> (Int -> Double) -> Arvy IndexMeanState r
indexMeanScore ty af = arvy @IndexMeanMessage ArvyInst
  { arvyInitiate = \i s -> do
      (indexMean, _) <- get
      w <- edgePart s
      return (IndexMeanMessage w (opoint (i, getIndexScore indexMean)))
  , arvyTransmit = \msg@(IndexMeanMessage w xs) i s -> do
      best <- select msg
      w' <- edgePart s
      indexMean <- gets fst
      let newMessage = IndexMeanMessage (w + w') ((i, getIndexScore indexMean) : map (first forward) xs)
      return (best, newMessage)
  , arvyReceive = \msg _ -> do
      best <- select msg
      modify (second (+1))
      return best
  } where

  select :: ArvySelector IndexMeanMessage IndexMeanState r
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


  getScore :: Double -> Maybe Double -> Double -> Double
  getScore _ Nothing weight = weight
  getScore a (Just iScore) weight = weight ** a * iScore ** (1 - a)

  edgePart :: Member (LocalWeights i) r => i -> Sem r Double
  edgePart = case ty of
    HopIndexBased -> \_ -> return 1
    WeightSumBased -> weightTo

data LocalMinPairsMessage i = LocalMinPairsMessage [(i, Double)] [UArray Int Double] deriving Show

localMinPairs :: (Member Trace r, Show s) => Arvy s r
localMinPairs = arvy @LocalMinPairsMessage ArvyInst
  { arvyInitiate = \i _ -> return (LocalMinPairsMessage [(i, 0)] [listArray (0, 0) [0]])
  , arvyTransmit = \(LocalMinPairsMessage nodes dists) i _ -> do
      let count = length nodes
      --trace $ "We are at count " ++ show count
      weights <- zipWith3 (\k (j, score) weight -> (k, j, score, weight * fromIntegral count + score)) [0 :: Int ..] nodes <$> traverse (weightTo.fst) nodes
      --trace $ "Scores determined to be " ++ show weights
      let (bestIndex, best, bestScore, _) = minimumBy (comparing (\(_, _, _, d) -> d)) weights
      bestWeight <- weightTo best
      --trace $ "Selected node at index " ++ show bestIndex ++ " with node score " ++ show bestScore ++ " and weight " ++ show bestWeight

      let getDist u v
            | u == v = 0
            | u > v = getDist v u
            | otherwise = dists !! v ! u

      let newWeights = listArray (0, count - 1) (map (\j -> getDist j bestIndex + bestWeight) [0 :: Int ..])
      --trace $ "New weights are " ++ show newWeights
      let newDists = dists ++ [newWeights]

      let newNodes = map (\(j, (n, s)) -> (forward n, s + newWeights ! j)) (zip [0 :: Int ..] nodes)
      --trace $ "Updated old node scores to " ++ show newNodes
      let newNodeScore = bestScore + fromIntegral count * bestWeight
      let newNodes' = newNodes ++ [(i, newNodeScore)]
      --trace $ "Added new node score " ++ show newNodeScore

      --let newNodes' =
      --let newArray = array ((0, 0), (count, count)) []

      return (best, LocalMinPairsMessage newNodes' newDists)
  , arvyReceive = \(LocalMinPairsMessage nodes _) _ -> do
      let count = length nodes
      --trace $ "We are at the final count " ++ show count
      weights <- zipWith (\(j, score) weight -> (j, weight * fromIntegral count + score)) nodes <$> traverse (weightTo.fst) nodes
      let best = fst $ minimumBy (comparing snd) weights
      return best
  }
