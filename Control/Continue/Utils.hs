-- |
-- Module:     Control.Continue.Utils
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Suspension/continuation utilities.

module Control.Continue.Utils
    ( -- * Basic utilities
      addCont_,
      continue,
      continue_,
      suspend
    )
    where

import Control.Continue.Class
import Data.Functor.Plus


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
