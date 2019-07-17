{-# LANGUAGE BangPatterns #-}

module Utils where

import Polysemy
import Polysemy.State
import Data.Bifunctor
import qualified Data.Random                   as R
import qualified Data.Random.Internal.Source   as R
import Polysemy.RandomFu
import Data.Array.IArray
import Arvy.Local
import qualified Data.Tree as T
import Data.Maybe
import Arvy.Algorithm
import Data.Array.MArray
import Data.Array.Base
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Random

-- | Convenience value for infinity for floating point types
infinity :: (Floating a, Read a) => a
infinity = read "Infinity"


-- | Like 'fix' but over a functor structure. See https://github.com/quchen/articles/blob/master/loeb-moeb.md
loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

{-# INLINABLE aimap #-}
-- | Returns a new array derived from the original array by applying a
-- function to each of the elements and their indices. Extension of 'Data.Array.Base.amap' with indices.
aimap :: (IArray a' e, IArray a e', Ix i) => (i -> e' -> e) -> a i e' -> a' i e
aimap f arr = array (bounds arr) $ (\(i, e) -> (i, f i e)) <$> assocs arr

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
    !old <- get
    put $ setter old v

-- TODO: Use lenses
-- | Transforms a stateful computation over @a@ to a computation over @s@ that holds a value of @a@.
mapState'
  :: (s -> a) -- ^ How to get an @a@ from @s@
  -> (s -> a -> s) -- ^ How to set the @a@ part in an @s@,
  -> Sem (State a ': r) x
  -> Sem (State s ': r) x
mapState' getter setter = reinterpret \case
  Get -> gets getter
  Put v -> do
    !old <- get
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

-- | Transforms a stateful computation over @a@ to a computation over @(a, b)
mapStateFirst'
  :: Sem (State a ': r) x
  -> Sem (State (a, b) ': r) x
mapStateFirst' = mapState' fst (flip $ first . const)

-- | Transforms a stateful computation over @b@ to a computation over @(a, b)
mapStateSecond'
  :: Sem (State b ': r) x
  -> Sem (State (a, b) ': r) x
mapStateSecond' = mapState' snd (flip $ second . const)


-- | Run a 'Random' effect using a given 'R.RandomSource'
runRandomSource'
  :: forall s r a m
   . (Member (Lift m) r, R.RandomSource m s)
  => s
  -> Sem (RandomFu ': r) a
  -> Sem r a
runRandomSource' source = interpret $ \case
    SampleRVar    rv -> sendM $ R.runRVar (R.sample rv) source
    GetRandomPrim pt -> sendM $ R.runRVar (R.getRandomPrim pt) source
{-# INLINEABLE runRandomSource' #-}

-- | Extracts the root and a map from nodes to their children from a 'RootedTree'
treeRootPred :: RootedTree -> (Node, IntMap IntSet)
treeRootPred tree = (root, predecessorMap) where

  (mroot, predecessorMap) = invert (assocs tree)
  root = fromMaybe (error "Tree has no root") mroot

  -- TODO: Use more efficient representation for predecessors, e.g. IntSet
  -- | Inverts an (node index, successor pointer) list to a (root, predecessor mapping) value
  invert :: [(Node, Node)] -> (Maybe Node, IntMap IntSet)
  invert []                   = (Nothing, IntMap.empty)
  invert ((i, pointer):rest) = if i == pointer
    then ( case root' of

             Nothing -> Just i
             Just i' -> error $ "Tree has multiple roots at both node " ++ show i ++ " and " ++ show i'
         , rest' )
    else ( root'
         , IntMap.insertWith IntSet.union pointer (IntSet.singleton i) rest' )
    where (root', rest') = invert rest

-- | Converts a rooted spanning tree in the form of a pointer array to a 'T.Tree' value, useful for processing or display with 'T.drawTree'.
-- Throws an error when there's multiple or no roots. Does *not* throw an error when some nodes don't transitively point to the root, instead those nodes are just not included in the final tree structure.
treeStructure :: RootedTree -> T.Tree Node
treeStructure tree = T.unfoldTree predecessors root where

  (root, predecessorMap) = treeRootPred tree

  predecessors :: Node -> (Node, [Node])
  predecessors node = (node, IntSet.toAscList $ IntMap.findWithDefault IntSet.empty node predecessorMap )


{-# INLINE floydWarshall #-}
-- TODO: Split a lot of these things out of this Arvy module into the arvy-eval component
-- | Does the main operation in the floyd-warshall algorithm. Computes the shortest path between all nodes by iteratively modifying given weights. Complexity /O(n^3)/
floydWarshall :: MArray arr Weight m => NodeCount -> GraphWeightsArr arr -> m ()
floydWarshall n weights =
  forM_ [0..n - 1] $ \k ->
    forM_ [0..n - 1] $ \i ->
      forM_ [0..n - 1] $ \j -> do
        ij <- unsafeRead weights (i * n + j)
        ik <- unsafeRead weights (i * n + k)
        kj <- unsafeRead weights (k * n + j)
        let ikj = ik + kj
        when (ij > ikj) $
          unsafeWrite weights (i * n + j) ikj

randomSetElement :: Set a -> RVar a
randomSetElement set = do
  let size = Set.size set
  r <- uniformT 0 (size - 1)
  return $ Set.elemAt r set
