{-# LANGUAGE BangPatterns #-}

module Utils.Parmap where

import Polysemy
import Control.DeepSeq
import Polysemy.Trace
import qualified Polysemy.Async as PA
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TSem
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad

parMap
  :: forall r a b
  . ( NFData b
    , Show a
    , Members '[Trace, Lift IO, PA.Async] r )
  => Int
  -> (a -> Sem r b)
  -> [a] -> Sem r [b]
parMap k f as = do

  inputs :: TQueue (Sem r b) <- sendM newTQueueIO
  count :: TVar Int <- sendM $ newTVarIO 0
  sendM $ atomically $ forM_ as $ \a -> do
    writeTQueue inputs (f a)
    modifyTVar' count (+1)
  c <- sendM $ readTVarIO count
  trace $ "Count is " ++ show c

  outputs :: TQueue b <- sendM newTQueueIO
  sem <- sendM $ atomically $ newTSem (fromIntegral k)
  let runJob :: Sem r [Async (Maybe ())]
      runJob = do
        trace "Trynig to get a job"
        job <- sendM $ atomically $ do
          val <- tryReadTQueue inputs
          case val of
            Nothing -> return ()
            Just _ -> waitTSem sem
          return val
        case job of
          Nothing -> do
            trace "No more jobs left"
            return []
          Just j -> do
            trace "Running job asynchronously"
            asyn <- PA.async $ do
              i <- sendM myThreadId
              trace (show i ++ " Separate thread, running job")
              !res <- force <$> j
              trace (show i ++ " Finished running job")
              sendM $ atomically $ do
                writeTQueue outputs res
                signalTSem sem
            (asyn:) <$> runJob
      readResults :: IO [b]
      readResults = do
        output <- atomically $ do
          c <- readTVar count
          if c > 0
            then do
              writeTVar count (c - 1)
              Just <$> readTQueue outputs
            else return Nothing
        case output of
          Nothing -> return []
          Just o -> (o:) <$> readResults


  asyncs <- runJob
  forM_ asyncs PA.await
  sendM readResults
