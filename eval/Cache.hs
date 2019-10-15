{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Cache where

import Data.Store
import Polysemy
import System.Directory
import Arvy.Log
import System.FilePath
import qualified Data.ByteString as BS

newtype CacheKey = CacheKey { unCacheKey :: FilePath }

instance Show CacheKey where
  show (CacheKey key) = key


cache :: forall a r . (LogMember r, Member (Lift IO) r, Store a) => CacheKey -> Sem r a -> Sem r a
cache key compute = do
  path <- sendM $ (</> unCacheKey key) <$> getXdgDirectory XdgCache "arvy"
  exists <- sendM $ doesFileExist path
  if exists then do
    contents <- sendM $ BS.readFile path
    case decode contents of
      Left err -> do
        lgError $ "Error decoding cache file " <> tshow path <> ": " <> tshow err
        computeAndStore path
      Right value -> do
        lgDebug $ "Reusing cached value for " <> tshow key
        return value
    else computeAndStore path
  where
    computeAndStore :: FilePath -> Sem r a
    computeAndStore path = do
      lgDebug $ "Computing " <> tshow key <> "..."
      !value <- compute
      let encoded = encode value
      sendM $ createDirectoryIfMissing True (takeDirectory path)
      sendM $ BS.writeFile path encoded
      return value
