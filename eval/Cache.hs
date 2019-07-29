{-# LANGUAGE BangPatterns #-}

module Cache where

import Data.Store
import Polysemy
import Polysemy.Trace
import System.Directory
import System.FilePath
import qualified Data.ByteString as BS

newtype CacheKey = CacheKey { unCacheKey :: FilePath }

instance Show CacheKey where
  show (CacheKey key) = key


cache :: forall a r . (Members '[Lift IO, Trace] r, Store a) => CacheKey -> Sem r a -> Sem r a
cache key compute = do
  path <- sendM $ (</> unCacheKey key) <$> getXdgDirectory XdgCache "arvy"
  exists <- sendM $ doesFileExist path
  if exists then do
    contents <- sendM $ BS.readFile path
    case decode contents of
      Left err -> do
        trace $ "Error decoding cache file " ++ show path ++ ": " ++ show err
        computeAndStore path
      Right value -> do
        trace $ "Reusing cached value for " ++ show key
        return value
    else computeAndStore path
  where
    computeAndStore :: FilePath -> Sem r a
    computeAndStore path = do
      trace $ "Computing " ++ show key ++ "..."
      !value <- compute
      let encoded = encode value
      sendM $ createDirectoryIfMissing True (takeDirectory path)
      sendM $ BS.writeFile path encoded
      return value
