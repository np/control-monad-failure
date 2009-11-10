{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Control.Monad.Failure
  (
  module Control.Monad.Failure.Class
  -- * 'StackFrame's
  , StackFrame(..)
  , stackFrame
  , failureAt
  , stackTrace
  ) where

import Control.Monad.Failure.Class
import Data.Typeable
import Data.Maybe (fromMaybe)
import qualified Control.Exception as E

-- ! 'StackFrame's decorate exceptions with some location data.
-- Note that when actually raising a 'StackFrame', this frame
-- is discarded and the underlying exception is raised.
-- However the 'StackTrace' is kept and augemented while used
-- in pure code. Using for instance 'MonadLoc', 'withLoc' and
-- 'withLocTH'.
data StackFrame = StackFrame { stackFrameText      :: String
                             , stackFrameException :: E.SomeException
                             }
  deriving (Typeable)

-- ! Builds a stack frame given a source location text and
-- an inner exception.
stackFrame :: E.Exception e => String -> e -> StackFrame
stackFrame loc = StackFrame loc . E.SomeException

-- | Extracts a stack trace from any exception. This basically
-- look for a chain of 'StackFrame's, and stops on any other
-- exception.
stackTrace :: E.Exception e => e -> [String]
stackTrace e =
  case cast e :: Maybe StackFrame of
    Nothing -> []
    Just (StackFrame loc (E.SomeException e')) -> loc : stackTrace e'

instance E.Exception StackFrame where
  toException sf = case stackFrameException sf of
                     E.SomeException e -> E.toException e
  fromException e = Just . fromMaybe (StackFrame "" e) . cast $ e

instance Show StackFrame where
  showsPrec p (StackFrame "" e)
    = showsPrec p e
  showsPrec p (StackFrame txt e)
    = showsPrec p e . showString "\n    at " . showString txt

-- | Like 'failure', but wraps the exception with a 'StackFrame'
-- of the given source location text.
failureAt :: (E.Exception e, MonadFailure StackFrame m) => String -> e -> m a
failureAt txt e = failure (StackFrame txt (E.SomeException e))
