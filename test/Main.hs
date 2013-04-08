-- |
-- Module:     Main
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Test program for the continue package.

module Main where

import Control.Continue
import Data.Conduit
import Data.Map (Map)
import Data.Monoid
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp


type App url a = ContinueT LastEx (Map url) (ResourceT IO) a


main :: IO ()
main =
    run 8000 $ \req -> do
        return (ResponseBuilder ok200 [] mempty)
