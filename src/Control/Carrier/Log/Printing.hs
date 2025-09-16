{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for the 'Log' effect that prints all logged messages using an 'IO' action.
module Control.Carrier.Log.Printing
( -- * Log carrier
  runLog
, runLogHandle
, runLogStdout
, runLogStderr
, runLogFile
, runLogPrintHandle
, runLogPrintStderr
, runLogPrint
, runLogPrintFile
, runLogPutStrHandle
, runLogPutStrStderr
, runLogPutStr
, runLogPutStrFile
, runLogPutStrLnHandle
, runLogPutStrLnStderr
, runLogPutStrLn
, runLogPutStrLnFile
, LogC(LogC)
  -- * Log effect
, module Control.Effect.Log
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.Reader
import Control.Effect.Log
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import System.IO

-- | Run a @'Log' l@ effect, executing the given 'IO' action on each log message.
--
-- @
-- 'runLog' f ('logMessage' s) = 'liftIO' (f s)
-- @
-- @
-- 'runLogWith' f (pure a) = 'pure' a
-- @
runLog :: MonadIO m => (l -> IO ()) -> LogC l m a -> m a
runLog logger = runReader logger . runLogC
{-# INLINE runLog #-}

-- | Run a @'Log' l@ effect, executing the given 'IO' action on each log message with a specified
-- 'Handle'.
runLogHandle :: MonadIO m => (Handle -> l -> IO ()) -> Handle -> LogC l m a -> m a
runLogHandle logger = runLog . logger
{-# INLINE runLogHandle #-}

-- | Run a @'Log' l@ effect, executing the given 'IO' action applied to the 'stdout' handle on each
-- log message.
runLogStdout :: MonadIO m => (Handle -> l -> IO ()) -> LogC l m a -> m a
runLogStdout logger = runLogHandle logger stdout
{-# INLINE runLogStdout #-}

-- | Run a @'Log' l@ effect, executing the given 'IO' action applied to the 'stderr' handle on each
-- log message.
runLogStderr :: MonadIO m => (Handle -> l -> IO ()) -> LogC l m a -> m a
runLogStderr logger = runLogHandle logger stderr
{-# INLINE runLogStderr #-}

-- | Run a @'Log' l@ effect, executing the given 'IO' action applied to a 'Handle' of the specified
-- file on each log message.
runLogFile :: (MonadIO m, MonadUnliftIO m) => (Handle -> l -> IO ()) -> FilePath -> IOMode -> LogC l m a -> m a
runLogFile logger filename mode m =
  withRunInIO $ \run ->
  withFile filename mode $ \handle ->
  run $ runLogHandle logger handle m
{-# INLINE runLogFile #-}

-- | Run a @'Log' l@ effect, executing 'hPrint' applied to the given 'Handle' on each log message.
runLogPrintHandle :: (Show l, MonadIO m) => Handle -> LogC l m a -> m a
runLogPrintHandle = runLogHandle hPrint
{-# INLINE runLogPrintHandle #-}

-- | Run a @'Log' l@ effect, executing 'print' on each log message.
runLogPrint :: (Show l, MonadIO m) => LogC l m a -> m a
runLogPrint = runLogPrintHandle stdout
{-# INLINE runLogPrint  #-}

-- | Run a @'Log' l@ effect, executing @'hPrint' 'stderr'@ on each log message.
runLogPrintStderr :: (Show l, MonadIO m) => LogC l m a -> m a
runLogPrintStderr = runLogPrintHandle stderr
{-# INLINE runLogPrintStderr #-}

-- | Run a @'Log' l@ effect, executing 'hPrint' applied to a 'Handle' of the specified file on each
-- log message.
runLogPrintFile :: (Show l, MonadIO m, MonadUnliftIO m) => FilePath -> IOMode -> LogC l m a -> m a
runLogPrintFile = runLogFile hPrint
{-# INLINE runLogPrintFile #-}

-- | Run a @'Log' 'String'@ effect, executing 'hPutStr' applied to the given 'Handle' on each log
-- message.
runLogPutStrHandle :: MonadIO m => Handle -> LogC String m a -> m a
runLogPutStrHandle = runLogHandle hPutStr
{-# INLINE runLogPutStrHandle   #-}

-- | Run a @'Log' 'String'@ effect, executing 'putStr' on each log message.
runLogPutStr :: MonadIO m => LogC String m a -> m a
runLogPutStr = runLogPutStrHandle stdout
{-# INLINE runLogPutStr  #-}

-- | Run a @'Log' 'String'@ effect, executing @'hPutStr' 'stderr'@ on each log message.
runLogPutStrStderr :: MonadIO m => LogC String m a -> m a
runLogPutStrStderr  = runLogPutStrHandle stderr
{-# INLINE runLogPutStrStderr   #-}

-- | Run a @'Log' 'String'@ effect, executing 'hPutStr' applied to a 'Handle' of the specified file
-- on each log message.
runLogPutStrFile :: (MonadIO m, MonadUnliftIO m) => FilePath -> IOMode -> LogC String m a -> m a
runLogPutStrFile = runLogFile hPutStr
{-# INLINE runLogPutStrFile #-}

-- | Run a @'Log' 'String'@ effect, executing 'hPutStrLn' applied to the given 'Handle' on each log message.
runLogPutStrLnHandle :: MonadIO m => Handle -> LogC String m a -> m a
runLogPutStrLnHandle = runLogHandle hPutStrLn
{-# INLINE runLogPutStrLnHandle  #-}

-- | Run a @'Log' 'String'@ effect, executing 'putStrLn' on each log message.
runLogPutStrLn :: MonadIO m => LogC String m a -> m a
runLogPutStrLn = runLogPutStrLnHandle stdout
{-# INLINE runLogPutStrLn  #-}

-- | Run a @'Log' 'String'@ effect, executing @'hPutStrLn' 'stderr'@ on each log message.
runLogPutStrLnStderr :: MonadIO m => LogC String m a -> m a
runLogPutStrLnStderr = runLogPutStrLnHandle stderr
{-# INLINE runLogPutStrLnStderr  #-}

-- | Run a @'Log' 'String'@ effect, executing 'hPutStrLn' applied to a 'Handle' of the specified file on
-- each log message.
runLogPutStrLnFile :: (MonadIO m, MonadUnliftIO m) => FilePath -> IOMode -> LogC String m a -> m a
runLogPutStrLnFile = runLogFile hPutStrLn
{-# INLINE runLogPutStrLnFile #-}

newtype LogC l m a = LogC { runLogC :: ReaderC (l -> IO ()) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadUnliftIO)

instance MonadTrans (LogC l) where
  lift = LogC . lift
  {-# INLINE lift #-}

instance (Algebra sig m, MonadIO m) => Algebra (Log l :+: sig) (LogC l m) where
  alg hdl sig ctx = LogC $ case sig of
    L (LogMessage msg) -> ReaderC $ \logger -> ctx <$ liftIO (logger msg)
    R other            -> alg (runLogC . hdl) (R other) ctx
  {-# INLINE alg #-}
