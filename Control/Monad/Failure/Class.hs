{-# LANGUAGE Rank2Types #-}
{-| Defines the class @MonadFailure@ for monads which can fail.
-}
module Control.Monad.Failure.Class where

import Control.Exception (Exception)


class Monad m => MonadFailure e m where
    failure :: e -> m a

class MonadFailure e m =>  WrapFailure e m where
    -- | Wrap the failure value, if any, with the given function. This is
    -- useful in particular when you want all the exceptions returned from a
    -- certain library to be of a certain type, even if they were generated by
    -- a different library.
    wrapFailure :: (forall eIn. Exception eIn => eIn -> e) -> m a -> m a