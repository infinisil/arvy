{-# LANGUAGE BlockArguments #-}

module Arvy.Utils where

import Data.Array.IArray
import Polysemy
import Polysemy.State
import Data.Bifunctor
import Algebra.Graph.Class
import Data.Graph.Generators

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
