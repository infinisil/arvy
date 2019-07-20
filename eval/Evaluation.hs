{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Evaluation where

import           Arvy.Algorithm
import           Arvy.Local
import           Control.Applicative
import           Control.Category
import           Control.DeepSeq
import           Data.Array.IO
import           Data.Bifunctor
import           Pipes
import           Polysemy
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.Trace
import           Prelude             hiding (id, (.))
import           System.IO
import           Utils

-- | Readonly data available to evaluations
data Env (arr :: * -> * -> *) = Env (arr Node Node)

---- | An 'Eval' is like a 'Tracer', but specific to 'ArvyEvent' inputs and the outputs get processed in some effect row @r@.
--data Eval r = forall s . Eval
--  { evalStart :: Sem r s
--  , evalRun :: Maybe ArvyEvent -> Sem (State s ': r) ()
--  , evalEnd :: Sem (State s ': r) ()
--  }
--
--{-# INLINE runEval #-}
--runEval :: (MArray arr Node m, Members '[Lift m] r) => Eval (Reader (Env arr) ': r) -> Env arr -> Sem (Output (Maybe ArvyEvent) ': r) () -> Sem r ()
--runEval Eval { .. } env sem = runReader env $ do
--  startState <- evalStart
--  fmap snd $ runState startState $ do
--    reinterpret2
--      \case Output !event -> evalRun event
--      sem
--    evalEnd
--
--instance Semigroup (Eval r) where
--  Eval st rn en <> Eval st' rn' en' = Eval
--    { evalStart = liftA2 (,) st st'
--    , evalRun = \event -> do
--        mapStateFirst' $ rn event
--        mapStateSecond' $ rn' event
--    , evalEnd = do
--        mapStateFirst' $ en
--        mapStateSecond' $ en'
--    }
--
--instance Monoid (Eval r) where
--  mempty = Eval
--    { evalStart = return ()
--    , evalRun = \_ -> return ()
--    , evalEnd = return ()
--    }
--
--
--data Runner o r where
--  RunTrace :: Member Trace r => (o -> String) -> Runner o r
--  RunFile :: Member (Lift IO) r => FilePath -> (o -> String) -> Runner o r
--
--
--
--runAs :: (MArray arr Node IO, Members '[Reader (Env arr), Lift IO] r) => Tracer ArvyEvent o -> Runner o r -> Eval r
--runAs (Tracer s t) (RunTrace f) = Eval
--  { evalStart = return (0 :: Int, s)
--  , evalRun = \event -> do
--      eventId <- gets fst
--      modify (first (+1))
--
--      interpret
--        \case Output o -> trace $ f o
--        (mapStateSecond (t event))
--  , evalEnd = return ()
--  }
--runAs (Tracer s t) (RunFile path f) = Eval
--  { evalStart = do
--      handle <- sendM $ openFile path WriteMode
--      return (handle, s)
--  , evalRun = \event -> do
--      interpret
--        \case Output o -> do
--                handle <- gets fst
--                sendM $ hPutStrLn handle (f o)
--        (mapStateSecond (t event))
--  , evalEnd = do
--      handle <- gets fst
--      sendM $ hClose handle
--  }
{-


-- | Function for coercing a 'Tracer' for 'ArvyEvent's into an 'Eval' with a function that acts upon the 'Tracer's outputs.
runTimingAs :: (MArray arr Node IO, Members '[Reader (Env arr), Lift IO] r) => Tracer ArvyEvent o -> (o -> Sem r ()) -> Eval r
runTimingAs (Tracer s t) f = Eval s $ \event -> do
  interpret
    \case Output o -> do
            start <- sendM getCurrentTime
            raise $ f o
            end <- sendM getCurrentTime
            sendM $ putStrLn $ "Time needed: " ++ show (end `diffUTCTime` start)
    (t event)
-}
