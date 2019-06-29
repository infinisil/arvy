-- Adapted from https://hackage.haskell.org/package/union-find-array-0.1.0.2/docs/Data-Union-ST.html

module Arvy.Utils.UnionFind where

import           Control.Monad.ST
import           Data.Array.ST
import Prelude hiding (lookup)
import Control.Monad (unless)

data UnionST s = UnionST
  { parent :: STUArray s Int Int
  , rank   :: STUArray s Int Int
  }

new :: Int -> ST s (UnionST s)
new n = UnionST <$> newListArray (0, n - 1) [0..] <*> newArray (0, n - 1) 0

lookup :: UnionST s -> Int -> ST s Int
lookup u@UnionST { .. } i = do
  i' <- readArray parent i
  if i == i' then return i else do
    i'' <- lookup u i'
    writeArray parent i i''
    return i''

merge :: UnionST s -> Int -> Int -> ST s ()
merge u@UnionST { .. } a b = do
  a' <- lookup u a
  b' <- lookup u b
  unless (a' == b') $ do
    ra <- readArray rank a'
    rb <- readArray rank b'
    case ra `compare` rb of
      LT ->
        writeArray parent a' b'
      GT ->
        writeArray parent b' a'
      EQ -> do
        writeArray parent a' b'
        writeArray rank b' (ra + 1)
