-- |
-- Module:     Control.Monad.Continue.Class
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Continue.Class
    ( -- * Suspension
      MonadContinue(..),

      -- * Basic utilities
      addCont_,
      continue,
      continue_,
      suspend,
      suspendWith
    )
    where

import qualified Control.Monad.Trans.State.Strict as Ss
import qualified Control.Monad.Trans.Writer.Strict as Ws
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Data.Functor.Plus
import Data.Monoid


-- | Type class for monads that support suspension and continuation
-- spots.

class (Plus f, Monad m, Monoid e) => MonadContinue e f m | m -> e, m -> f where
    -- | Add the given set of continuations and possibly suspend.
    addCont ::
        Either e a       -- ^ What to return now (left suspends).
        -> f (m a)  -- ^ What to run and return when reentering.
        -> m a

instance (MonadContinue e f m) => MonadContinue e f (IdentityT m) where
    addCont mx c = IdentityT $ addCont mx (fmap runIdentityT c)

instance (MonadContinue e f m) => MonadContinue e f (MaybeT m) where
    addCont mx c =
        MaybeT $
            addCont (fmap Just mx) (fmap runMaybeT c)

instance (MonadContinue e f m) => MonadContinue e f (ReaderT r m) where
    addCont mx c =
        ReaderT $ \env ->
            addCont mx (fmap (flip runReaderT env) c)

-- | Time travel warning: Captures the current state, not the state at
-- reentry.  Use 'ContinueT' over 'StateT' instead to fix this.

instance (MonadContinue e f m) => MonadContinue e f (StateT s m) where
    addCont mx c =
        StateT $ \s ->
            addCont (fmap (flip (,) s) mx)
                    (fmap (flip runStateT s) c)

-- | Time travel warning: Captures the current state, not the state at
-- reentry.  Use 'ContinueT' over 'StateT' instead to fix this.

instance (MonadContinue e f m) => MonadContinue e f (Ss.StateT s m) where
    addCont mx c =
        Ss.StateT $ \s ->
            addCont (fmap (flip (,) s) mx)
                    (fmap (flip Ss.runStateT s) c)

instance (MonadContinue e f m, Monoid l) => MonadContinue e f (WriterT l m) where
    addCont mx c =
        WriterT $
            addCont (fmap (flip (,) mempty) mx)
                    (fmap runWriterT c)

instance (MonadContinue e f m, Monoid l) => MonadContinue e f (Ws.WriterT l m) where
    addCont mx c =
        Ws.WriterT $
            addCont (fmap (flip (,) mempty) mx)
                    (fmap Ws.runWriterT c)


-- | Add the given set of continuations without suspending.

addCont_ :: (MonadContinue e f m) => f (m ()) -> m ()
addCont_ = addCont (Right ())


-- | Allow to continue here with the given value.

continue ::
    (MonadContinue e f m)
    => Either e a      -- ^ What to return now (left suspends).
    -> f (Either e a)  -- ^ What to return when reentering (left suspends).
    -> m a
continue mx = addCont mx . fmap (\mx -> addCont mx zero)


-- | Allow to continue here.

continue_ ::
    (MonadContinue e f m)
    => f ()  -- ^ Reentering key.
    -> m ()
continue_ = addCont (Right ()) . fmap (const $ return ())


-- | Suspend with the given value.  Does not register any continuation
-- spots.  Note that @suspend mempty@ is equivalent to @empty@.

suspend :: (MonadContinue e f m) => e -> m a
suspend ex = addCont (Left ex) zero


-- | Suspend with 'mempty' and register the given continuations.  Note
-- that @suspendWith zero@ is equivalent to @empty@.

suspendWith :: (MonadContinue e f m) => f (m a) -> m a
suspendWith = addCont (Left mempty)
