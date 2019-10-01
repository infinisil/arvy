{-# LANGUAGE ConstraintKinds #-}
module Arvy.Log
  ( module Colog.Core.Severity
  , Log(..)
  , lgDebug
  , lgInfo
  , lgWarning
  , lgError
  , runLogBySeverity
  , runLogBySeverity'
  , runLogBySeverity''
  , tshow
  , defaultLogAction
  , LogMember
  , runIgnoringLog
  ) where

import           Colog.Core.Severity
import           Colog.Message
import           Colog
import           Data.Text
import           Polysemy
import GHC.Stack
import Polysemy.Internal
import Polysemy.Internal.Union
import Control.Monad.IO.Class
import Control.Monad

data Log (m :: * -> *) a where
  LogDebug :: CallStack -> Text -> Log m ()
  LogInfo :: CallStack -> Text -> Log m ()
  LogWarning :: CallStack -> Text -> Log m ()
  LogError :: CallStack -> Text -> Log m ()

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

{-# INLINE runLogBySeverity' #-}
runLogBySeverity' :: Severity -> LogAction (Sem r) Message -> Sem (Log ': r) x -> Sem r x
runLogBySeverity' sev (LogAction action) = interpret $ \case
  LogDebug messageStack messageText -> when (Debug >= sev) $ action Message { messageSeverity = Debug, .. }
  LogInfo messageStack messageText -> when (Info >= sev) $ action Message { messageSeverity = Info, .. }
  LogWarning messageStack messageText -> when (Warning >= sev) $ action Message { messageSeverity = Warning, .. }
  LogError messageStack messageText -> when (Error >= sev) $ action Message { messageSeverity = Error, .. }

{-# INLINE runLogBySeverity'' #-}
runLogBySeverity'' :: forall r x . Severity -> LogAction (Sem r) Message -> Sem (Log ': r) x -> Sem r x
runLogBySeverity'' sev (LogAction action) = interpret $ \case
  LogDebug messageStack messageText -> debug Message { messageSeverity = Debug, .. }
  LogInfo messageStack messageText -> info Message { messageSeverity = Info, .. }
  LogWarning messageStack messageText -> warning Message { messageSeverity = Warning, .. }
  LogError messageStack messageText -> err Message { messageSeverity = Error, .. }
  where
    debug msg = when (Debug >= sev) $ action msg
    info msg = when (Info >= sev) $ action msg
    warning msg = when (Warning >= sev) $ action msg
    err msg = when (Error >= sev) $ action msg

runIgnoringLog :: Sem (Log ': r) x -> Sem r x
runIgnoringLog = interpret $ \case
  LogDebug _ _ -> return ()
  LogInfo _ _ -> return ()
  LogWarning _ _ -> return ()
  LogError _ _ -> return ()

tshow :: Show a => a -> Text
tshow = pack . show

defaultLogAction :: MonadIO m => LogAction m Message
defaultLogAction = cmap fmtMessage logTextStdout
