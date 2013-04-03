-- |
-- Module:     Control.Continue
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Proxy to the various modules of the continue package.

module Control.Continue
    ( -- * Continue modules
      module Control.Continue.Suspend,
      module Control.Continue.Types,

      -- * Convenience reexports
      Alt(..),
      Plus(..)
    )
    where

import Control.Continue.Suspend
import Control.Continue.Types
import Data.Functor.Plus
