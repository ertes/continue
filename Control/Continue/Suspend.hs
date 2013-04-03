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
      suspend_
    )
    where

import Control.Continue.Types
import Data.Functor.Plus
import Data.Monoid


-- | Add the given set of continuations without suspending.

addCont ::
    (Monad m)
    => a    -- ^ What to return now.
    -> f (Continue e f m a)  -- ^ What to run and return when reentering.
    -> Continue e f m a
addCont x c = Continue (return (Right x, c))


-- | Add the given set of continuations without suspending.

addCont_ :: (Monad m) => f (Continue e f m ()) -> Continue e f m ()
addCont_ = addCont ()


-- | Continue here with the given value.

continue ::
    (Monad m, Monoid e, Plus f)
    => a    -- ^ What to return now.
    -> f a  -- ^ What to return when reentering.
    -> Continue e f m a
continue x c =
    Continue (return (Right x, fmap return c))


-- | Continue here.

continue_ ::
    (Monad m, Monoid e, Plus f)
    => f ()  -- ^ What to return on reentering.
    -> Continue e f m ()
continue_ = continue ()


-- | Suspend with the given value.

suspend :: (Monad m, Plus f) => e -> Continue e f m a
suspend ex = Continue (return (Left ex, zero))


-- | Suspend with 'mempty'.

suspend_ :: (Monad m, Monoid e, Plus f) => Continue e f m a
suspend_ = Continue (return (Left mempty, zero))
