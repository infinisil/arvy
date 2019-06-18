module Arvy.Utils where

import Data.Array.IArray
import Control.DeepSeq
import Data.Array.MArray
import Data.Array.Unsafe
import Polysemy

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

{-# INLINE amap' #-}
-- | Returns a new array derived from the original array by applying a
-- function to each of the elements.
amap' :: (IArray a e', IArray a e, Ix i) => (i -> e' -> e) -> a i e' -> a i e
amap' f arr = array (bounds arr) $ (\(i, e) -> (i, f i e)) <$> assocs arr

infinity :: Double
infinity = read "Infinity"


-- | Temporarily get access to an immutable version of a mutable array. Only safe if this is the only thread that has access to the given mutable array.
{-# INLINE withFrozen #-}
withFrozen :: forall b m x a i e r . (Member (Lift m) r, NFData x, Ix i, MArray a e m, IArray b e) => a i e -> (forall y . (y -> e) -> b i y -> Sem r x) -> Sem r x
withFrozen arr fun = do
  freezed <- sendM @m (unsafeFreeze arr)
  res <- fun id freezed
  return $!! res
