module Arvy.Utils where

import           Data.Array.IArray

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

{-# INLINE amap' #-}
-- | Returns a new array derived from the original array by applying a
-- function to each of the elements.
amap' :: (IArray a e', IArray a e, Ix i) => (i -> e' -> e) -> a i e' -> a i e
amap' f arr = array (bounds arr) $ (\(i, e) -> (i, f i e)) <$> assocs arr
