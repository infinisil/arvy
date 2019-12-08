{-# LANGUAGE ConstraintKinds #-}
{- |
Description : Arvy logging effect
Copyright   : (c) Silvan Mosberger, 2019
License     : GPL-3
Maintainer  : contact@infinisil.com
Stability   : experimental

This module is the missing piece of infrastructure for combining polysemy with co-log
-}
module Arvy.Log
  ( module Colog.Core.Severity
  , Log(..)
  , lgDebug
  , lgInfo
  , lgWarning
  , lgError
  , runLogBySeverity
  , tshow
  , defaultLogAction
  , LogMember
  , runIgnoringLog
  ) where

import           Colog
import           Colog.Core.Severity
import           Control.Monad.IO.Class
import           Data.Text
import           GHC.Stack
import           Polysemy
import           Polysemy.Internal
import           Polysemy.Internal.Union

-- | A logging effect
data Log (m :: * -> *) a where
  LogDebug :: CallStack -> Text -> Log m ()
  LogInfo :: CallStack -> Text -> Log m ()
  LogWarning :: CallStack -> Text -> Log m ()
  LogError :: CallStack -> Text -> Log m ()

-- | A constraint for both providing a call stack and a logging effect
type LogMember r = (HasCallStack, MemberWithError Log r)

{-# INLINE lgDebug #-}
lgDebug :: LogMember r => Text -> Sem r ()
lgDebug text = withFrozenCallStack $ send (LogDebug callStack text)

{-# INLINE lgInfo #-}
lgInfo :: LogMember r => Text -> Sem r ()
lgInfo text = withFrozenCallStack $ send (LogInfo callStack text)

{-# INLINE lgWarning #-}
lgWarning :: LogMember r => Text -> Sem r ()
lgWarning text = withFrozenCallStack $ send (LogWarning callStack text)

{-# INLINE lgError #-}
lgError :: LogMember r => Text -> Sem r ()
lgError text = withFrozenCallStack $ send (LogError callStack text)

-- | A logging interpreter that only logs messages above a certain severity
-- All lower severity messages should be optimized away
{-# INLINE runLogBySeverity #-}
runLogBySeverity :: Severity -> LogAction (Sem r) Message -> Sem (Log ': r) x -> Sem r x
runLogBySeverity Debug (LogAction action) = interpret $ \case
  LogDebug messageStack messageText -> action Message { messageSeverity = Debug, .. }
  LogInfo messageStack messageText -> action Message { messageSeverity = Info, .. }
  LogWarning messageStack messageText -> action Message { messageSeverity = Warning, .. }
  LogError messageStack messageText -> action Message { messageSeverity = Error, .. }
runLogBySeverity Info (LogAction action) = interpret $ \case
  LogDebug _ _ -> return ()
  LogInfo messageStack messageText -> action Message { messageSeverity = Info, .. }
  LogWarning messageStack messageText -> action Message { messageSeverity = Warning, .. }
  LogError messageStack messageText -> action Message { messageSeverity = Error, .. }
runLogBySeverity Warning (LogAction action) = interpret $ \case
  LogDebug _ _ -> return ()
  LogInfo _ _ -> return ()
  LogWarning messageStack messageText -> action Message { messageSeverity = Warning, .. }
  LogError messageStack messageText -> action Message { messageSeverity = Error, .. }
runLogBySeverity Error (LogAction action) = interpret $ \case
  LogDebug _ _ -> return ()
  LogInfo _ _ -> return ()
  LogWarning _ _ -> return ()
  LogError messageStack messageText -> action Message { messageSeverity = Error, .. }

-- | A logging interpreter that ignores all messages
{-# INLINE runIgnoringLog #-}
runIgnoringLog :: Sem (Log ': r) x -> Sem r x
runIgnoringLog = interpret $ \case
  LogDebug _ _ -> return ()
  LogInfo _ _ -> return ()
  LogWarning _ _ -> return ()
  LogError _ _ -> return ()

-- | A convenience function for showing a value as Text
tshow :: Show a => a -> Text
tshow = pack . show

-- | The default logging action
defaultLogAction :: MonadIO m => LogAction m Message
defaultLogAction = cmap fmtMessage logTextStdout
