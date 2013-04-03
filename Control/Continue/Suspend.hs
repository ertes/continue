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
      suspend
    )
    where

import Control.Continue.Types
import Data.Functor.Plus


-- | Add the given set of continuations.  Does not suspend.

addCont :: (Monad m) => f (Continue e f m ()) -> Continue e f m ()
addCont c = Continue (return (Right (), c))


-- | Suspend with the given value.

suspend :: (Monad m, Plus f) => e -> Continue e f m a
suspend ex = Continue (return (Left ex, zero))
