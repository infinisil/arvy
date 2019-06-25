{-# LANGUAGE BlockArguments #-}

module Arvy.Utils where

import Data.Array.IArray
import Polysemy
import Polysemy.State
import Data.Bifunctor
import Algebra.Graph.Class
import Data.Graph.Generators

import qualified Algebra.Graph as G
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty
import qualified Algebra.Graph.ToGraph as T
import qualified Algebra.Graph.NonEmpty as N
import Control.Applicative ((<|>))
import Data.Function (on)

-- | Like 'fix' but over a functor structure. See https://github.com/quchen/articles/blob/master/loeb-moeb.md
loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

{-# INLINABLE aimap #-}
-- | Returns a new array derived from the original array by applying a
-- function to each of the elements and their indices. Extension of 'Data.Array.Base.amap' with indices.
aimap :: (IArray a e, IArray a e', Ix i) => (i -> e' -> e) -> a i e' -> a i e
aimap f arr = array (bounds arr) $ (\(i, e) -> (i, f i e)) <$> assocs arr

-- | Convenience value for infinity for floating point types
infinity :: (Floating a, Read a) => a
infinity = read "Infinity"


-- TODO: Use lenses
-- | Transforms a stateful computation over @a@ to a computation over @s@ that holds a value of @a@.
mapState
  :: Member (State s) r
  => (s -> a) -- ^ How to get an @a@ from @s@
  -> (s -> a -> s) -- ^ How to set the @a@ part in an @s@, 
  -> Sem (State a ': r) x
  -> Sem r x
mapState getter setter = interpret \case
  Get -> gets getter
  Put v -> do
    old <- get
    put $ setter old v

-- | Transforms a stateful computation over @a@ to a computation over @(a, b)
mapStateFirst
  :: Member (State (a, b)) r
  => Sem (State a ': r) x
  -> Sem r x
mapStateFirst = mapState fst (flip $ first . const)

-- | Transforms a stateful computation over @b@ to a computation over @(a, b)
mapStateSecond
  :: Member (State (a, b)) r
  => Sem (State b ': r) x
  -> Sem r x
mapStateSecond = mapState snd (flip $ second . const)

-- | Convert a 'GraphInfo' from graph-generators to a 'Graph' from algebraic-graphs
infoToGraph :: (Graph g, Vertex g ~ Int) => GraphInfo -> g
infoToGraph (GraphInfo { Data.Graph.Generators.edges = e }) = Algebra.Graph.Class.edges e


-- isConnected implementation taken from https://github.com/snowleopard/alga/pull/216

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
