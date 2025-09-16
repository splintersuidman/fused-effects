{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- | A variant of 'Control.Effect.Trace.Trace' that is polymorphic in the log message type.

Predefined carriers:

* "Control.Carrier.Log.Printing", which logs using a specified 'IO' action in a 'Control.Monad.IO.Class.MonadIO' context.
* "Control.Carrier.Log.Returning", which aggregates all logged messages a 'Monoid'.
* "Control.Carrier.Log.Ignoring", which discards all logged messages.

@since 0.1.0.0
-}

module Control.Effect.Log
( -- * Log effect
  Log(..)
, logMessage
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Data.Kind (Type)

data Log l (m :: Type -> Type) k where
  LogMessage :: { message :: l } -> Log l m ()

-- | Log a message.
logMessage :: Has (Log l) sig m => l -> m ()
logMessage message = send (LogMessage message)
{-# INLINE logMessage #-}
