{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Control.Monad.Failure
  (
  module Control.Monad.Failure.Class
  -- * 'StackFrame's
  , StackFrame(..)
  , stackFrame
  , failureAt
  ) where

import Control.Monad.Failure.Class
import Data.Typeable
import qualified Control.Exception as E

-- ! 'StackFrame's decorate exceptions with some location data.
-- Note that when actually raising a 'StackFrame', this frame
-- is discarded and the underlying exception is raised.
-- However the 'StackTrace' is kept and augemented while used
-- in pure code. Using for instance 'MonadLoc', 'withLoc' and
-- 'withLocTH'.
data StackFrame e = StackFrame { stackFrameText      :: String
                               , stackFrameException :: e
                               }
  deriving (Eq, Ord, Typeable)

-- ! Builds a stack frame given a source location text and
-- an inner exception.
stackFrame :: E.Exception e => String -> e -> StackFrame e
stackFrame = StackFrame

instance E.Exception e => E.Exception (StackFrame e) where
  toException   = E.toException . stackFrameException
  fromException
    = ((fmap (stackFrame "") . E.fromException) =<<) . cast

instance Show e => Show (StackFrame e) where
  showsPrec p (StackFrame "" e)
    = showsPrec p e
  showsPrec p (StackFrame txt e)
    = showsPrec p e . showString "\n    at " . showString txt

-- | Like 'failure', but wraps the exception with a 'StackFrame'
-- of the given source location text.
failureAt :: MonadFailure (StackFrame e) m => String -> e -> m a
failureAt txt e = failure (StackFrame txt e)
