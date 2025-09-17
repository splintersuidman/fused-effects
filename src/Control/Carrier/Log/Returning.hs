{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for the 'Log' effect that aggregates and returns all logged messages.
module Control.Carrier.Log.Returning
( -- * Log carrier
  runLogWith
, runLog
, runLogList
, LogC(LogC)
  -- * Log effect
, module Control.Effect.Log
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.State.Strict
import Control.Effect.Log
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Bifunctor (first)

-- | Run a @'Log' l@ effect, transforming each message to a 'Monoid' @o@ via the given function, and
-- 'mappend'ing all resulting values.
--
-- @
-- 'runLogWith' f ('logMessage' s) = 'pure' (f s, ())
-- @
-- @
-- 'runLogWith' f (pure a) = 'pure' ('mempty', a)
-- @
-- @
-- 'runLogWith' @'Int' 'Data.Semigroup.Sum' ('logMessage' (1 :: 'Int') >> 'logMessage' (2 :: 'Int')) = 'pure' ('Data.Semigroup.Sum' 3, ())
-- @
runLogWith :: forall l o m a. (Monoid o, Functor m) => (l -> o) -> LogC l o m a -> m (o, a)
runLogWith f = fmap (first snd) . runState (f, mempty) . runLogC
{-# INLINE runLogWith #-}

-- | Run a @'Log' l@ effect with @l@ a 'Monoid', 'mappend'ing all log messages.
--
-- @
-- 'runLog' = 'runLogWith' 'id'
-- @
runLog :: forall l m a. (Monoid l, Functor m) => LogC l l m a -> m (l, a)
runLog = runLogWith id
{-# INLINE runLog #-}

-- | Run a 'Log' effect, returning all log messages as a list.
--
-- @
-- 'runLogList' = 'runLogWith' (: [])
-- @
runLogList :: forall l m a. Functor m => LogC l [l] m a -> m ([l], a)
runLogList = runLogWith (: [])
{-# INLINE runLogList #-}

newtype LogC l o m a = LogC { runLogC :: StateC (l -> o, o) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance (Monoid o, Algebra sig m) => Algebra (Log l :+: sig) (LogC l o m) where
  alg hdl sig ctx = LogC $ case sig of
    L (LogMessage msg) -> StateC $ \(f, o) -> do
      let !o' = mappend o (f msg)
      pure ((f, o'), ctx)
    R other            -> alg (runLogC . hdl) (R other) ctx
  {-# INLINE alg #-}
