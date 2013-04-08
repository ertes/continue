-- |
-- Module:     Control.Continue
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Proxy to the various modules of the continue package.

module Control.Continue
    ( -- * Continue modules
      module Control.Continue.Class,
      module Control.Continue.Types,
      module Control.Continue.Utils,

      -- * Convenience reexports
      Alt(..),
      Plus(..)
    )
    where

import Control.Continue.Class
import Control.Continue.Types
import Control.Continue.Utils
import Data.Functor.Plus
