-- |
-- Module:     Main
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Test program for the continue package.

module Main where

import Control.Applicative
import Control.Continue
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Map (Map)
import Happstack.Server


newtype App m a =
    App {
      runApp :: ContinueT LastEx (Map String) (ServerPartT m) a
    }
    deriving (Applicative, Functor, Monad)

deriving instance (MonadBase b m) => MonadBase b (App m)

instance (MonadBaseControl b m) => MonadBaseControl b (App m) where
    newtype StM (App m) a = StApp (StM (ContinueT LastEx (Map String) (ServerPartT m)) a)

    liftBaseWith k =
        App . liftBaseWith $ \runC ->
            k (liftM StApp . runC . runApp)

    restoreM (StApp c) = App (restoreM c)

instance MonadTrans App where
    lift = App . lift . lift


main :: IO ()
main =
    simpleHTTP nullConf (return "ok")
