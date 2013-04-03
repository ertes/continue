-- |
-- Module:     Control.Continue.Suspend
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Suspension utilities.

module Control.Continue.Suspend
    ( -- * Basic utilities
      addCont,
      addCont_,
      continue,
      continue_,
      suspend,
    )
    where

import Control.Continue.Types
import Data.Functor.Plus
import Data.Monoid


-- | Add the given set of continuations and possibly suspend.

addCont ::
    (Monad m)
    => Either e a    -- ^ What to return now (left suspends).
    -> f (ContinueT e f m a)  -- ^ What to run and return when reentering.
    -> ContinueT e f m a
addCont mx cf = ContinueT (return (mx, cf))


-- | Add the given set of continuations without suspending.

addCont_ :: (Monad m) => f (ContinueT e f m ()) -> ContinueT e f m ()
addCont_ = addCont (Right ())


-- | Allow to continue here with the given value.

continue ::
    (Monad m, Monoid e, Plus f)
    => Either e a      -- ^ What to return now (left suspends).
    -> f (Either e a)  -- ^ What to return when reentering (left suspends).
    -> ContinueT e f m a
continue mx c =
    ContinueT (return (mx, fmap (\mx -> ContinueT (return (mx, zero))) c))


-- | Allow to continue here.

continue_ ::
    (Monad m, Monoid e, Plus f)
    => f ()  -- ^ Reentering key.
    -> ContinueT e f m ()
continue_ c = continue (Right ()) (fmap Right c)


-- | Suspend with the given value.  Does not register any continuation
-- spots.  Note that @suspend mempty@ is equivalent to @empty@.

suspend :: (Monad m, Plus f) => e -> ContinueT e f m a
suspend ex = ContinueT (return (Left ex, zero))
