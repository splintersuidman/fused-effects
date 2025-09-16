{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for the 'Log' effect that ignores all log messages.
module Control.Carrier.Log.Ignoring
( -- * Log carrier
  runLog
, LogC(..)
  -- * Log effect
, module Control.Effect.Log
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Effect.Log
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Log' effect, ignoring all log messages.
--
-- @
-- 'runLog' ('logMessage' s) = 'pure' ()
-- @
-- @
-- 'runLog' ('pure' a) = 'pure' a
-- @
runLog :: LogC l m a -> m a
runLog (LogC m) = m
{-# INLINE runLog #-}

newtype LogC l m a = LogC (m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (LogC l) where
  lift = LogC
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Log l :+: sig) (LogC l m) where
  alg hdl = \case
    L (LogMessage _) -> pure
    R other -> LogC . alg (runLog . hdl) other
  {-# INLINE alg #-}
