module Utils.Connected where

import qualified Algebra.Graph as G
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty
import qualified Algebra.Graph.ToGraph as T
import qualified Algebra.Graph.NonEmpty as N
import Control.Applicative ((<|>))
import Data.Function (on)

-- Implementation taken from https://github.com/snowleopard/alga/pull/216

sharesVertex :: (T.ToGraph t, Ord (T.ToVertex t)) => t -> t -> Bool
sharesVertex = not ... (Set.disjoint `on` T.vertexSet) where
  (...) = (.) . (.)

components :: Ord a => G.Graph a -> Set.Set (N.Graph a)
components = ccs where
  underList :: Ord a => ([a] -> [a]) -> (Set.Set a -> Set.Set a)
  underList = (Set.fromList .) . (. Set.toList)

  toMaybe :: Bool -> a -> Maybe a
  toMaybe False _ = Nothing
  toMaybe True  x = Just x

  rewrite :: (a -> Maybe a) -> (a -> a)
  rewrite f x = case f x of
    Nothing -> x
    Just x' -> rewrite f x'

  rewrites2 :: (a -> a -> Maybe a) -> ([a] -> [a])
  rewrites2 f = rewrite rewrites2'
      where
        rewrites2' (x1 : x2 : xs) =
          ((: xs) <$> f x1 x2) <|>
          ((x2 :) <$> rewrites2' (x1 : xs)) <|>
          ((x1 :) <$> rewrites2' (x2 : xs))
        rewrites2' _ = Nothing

  nonEmptyCCs :: Ord a => G.Graph a -> Maybe (NonEmpty.NonEmpty (N.Graph a))
  nonEmptyCCs = NonEmpty.nonEmpty . Set.toList . ccs

  ccs :: Ord a => G.Graph a -> Set.Set (N.Graph a)
  ccs G.Empty = Set.empty
  ccs (G.Vertex x) = Set.singleton $ N.Vertex x
  ccs (G.Overlay a b) = underList
    (rewrites2 $ \g1 g2 -> toMaybe (sharesVertex g1 g2) (N.overlay g1 g2))
    (ccs a <> ccs b)
  ccs (G.Connect a b) = case nonEmptyCCs a of
    Nothing -> ccs b
    Just ca -> case nonEmptyCCs b of
      Nothing -> Set.fromList $ NonEmpty.toList ca
      Just cb -> Set.singleton $ N.overlays1 $ N.connect <$> ca <*> cb


-- | Does the graph have exactly one connected component?
-- @
-- isConnected empty == False
-- isConnected $ vertex x == True
-- isConnected $ vertex x + vertex y == False
-- @
isConnected :: Ord a => G.Graph a -> Bool
isConnected g = case Set.toList $ components g of
  [_] -> True
  _   -> False
