module Utils where

import Polysemy
import Polysemy.State
import Data.Bifunctor
import qualified Data.Random                   as R
import qualified Data.Random.Internal.Source   as R
import Polysemy.RandomFu
import Data.Array.IArray
import qualified Data.Map as M
import Arvy.Local
import qualified Data.Tree as T
import Data.Maybe

-- | Convenience value for infinity for floating point types
infinity :: (Floating a, Read a) => a
infinity = read "Infinity"


-- | Like 'fix' but over a functor structure. See https://github.com/quchen/articles/blob/master/loeb-moeb.md
loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

{-# INLINABLE aimap #-}
-- | Returns a new array derived from the original array by applying a
-- function to each of the elements and their indices. Extension of 'Data.Array.Base.amap' with indices.
aimap :: (IArray a e, IArray a e', Ix i) => (i -> e' -> e) -> a i e' -> a i e
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


-- | Converts a rooted spanning tree in the form of a pointer array to a 'T.Tree' value, useful for processing or display with 'T.drawTree'.
-- Throws an error when there's multiple or no roots. Does *not* throw an error when some nodes don't transitively point to the root, instead those nodes are just not included in the final tree structure.
treeStructure :: RootedTree -> T.Tree Node
treeStructure tree = T.unfoldTree predecessors root where

  predecessors :: Node -> (Node, [Node])
  predecessors node = (node, M.findWithDefault [] node predecessorMap )

  (mroot, predecessorMap) = invert (assocs tree)
  root = fromMaybe (error "Tree has no root") mroot

  -- TODO: Use more efficient representation for predecessors, e.g. IntSet
  -- | Inverts an (node index, successor pointer) list to a (root, predecessor mapping) value
  invert :: [(Node, Maybe Node)] -> (Maybe Node, M.Map Node [Node])
  invert []                   = (Nothing, M.empty)
  invert ((i, pointer):rest) = case pointer of
    Nothing        -> (case root' of
                         Nothing -> Just i
                         Just i' -> error $ "Tree has multiple roots at both node " ++ show i ++ " and " ++ show i'
                      , rest')
    Just successor -> (root'
                      , M.insertWith (++) successor [i] rest')
    where (root', rest') = invert rest
