-- |
-- Module:     Control.Continue.Types
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Types used in the continue library.

{-# LANGUAGE UndecidableInstances #-}

module Control.Continue.Types
    ( -- * Suspendable computations
      ContinueT(..),
      Continue,

      -- * Convenience types
      LastEx
    )
    where

import qualified Data.Bifunctor as Bi
import Control.Applicative
import Control.Arrow
import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Functor.Plus
import Data.Monoid


-- | This monad transformer adds suspensions under @f@ to @m@.

newtype ContinueT e f m a =
    ContinueT {
      runContinueT :: m (Either e a, f (ContinueT e f m a))
    }

instance (Monad m, Monoid e, Plus f) => Alternative (ContinueT e f m) where
    empty = ContinueT (return (Left mempty, zero))

    ContinueT c1 <|> ContinueT c2 =
        ContinueT $ do
            (mx1, cf1) <- c1
            (mx2, cf2) <- c2
            let mx = either (\ex1 -> Bi.first (ex1 <>) mx2) Right mx1
            return (mx, cf1 <!> cf2)

instance (Monad m, Plus f) => Applicative (ContinueT e f m) where
    pure x = ContinueT (return (Right x, zero))

    ContinueT cf <*> rx@(ContinueT cx) =
        ContinueT $ do
            (mf, cff) <- cf
            case mf of
              Left ex -> return (Left ex, fmap (<*> rx) cff)
              Right f -> do
                  (mx, cfx) <- cx
                  return (fmap f mx, fmap (fmap f) cfx)

instance (Functor f, Monad m) => Functor (ContinueT e f m) where
    fmap f (ContinueT c) = ContinueT (liftM (fmap f *** fmap (fmap f)) c)

instance (Monad m, Monoid e, Plus f) => Monad (ContinueT e f m) where
    return = pure

    ContinueT c >>= f =
        ContinueT $ do
            (mx, cfx') <- c
            let cfx = fmap (>>= f) cfx'
            case mx of
              Left ex -> return (Left ex, cfx)
              Right x -> do
                  (my, cfy) <- runContinueT (f x)
                  return (my, cfx <!> cfy)

    fail _ = ContinueT (return (Left mempty, zero))

instance (MonadBase b m, Monoid e, Plus f) => MonadBase b (ContinueT e f m) where
    liftBase = liftBaseDefault

instance (MonadFix m, Monoid e, Plus f) => MonadFix (ContinueT e f m) where
    mfix f =
        ContinueT . mfix $ \ ~(mx, _) ->
            runContinueT . f .
            either (const $ error "Feedback broken by suspension") id $ mx

instance (MonadIO m, Monoid e, Plus f) => MonadIO (ContinueT e f m) where
    liftIO = ContinueT . liftM (\x -> (Right x, zero)) . liftIO

instance (Monad m, Monoid e, Plus f) => MonadPlus (ContinueT e f m) where
    mzero = empty
    mplus = (<|>)

instance (Plus f) => MonadTrans (ContinueT e f) where
    lift = ContinueT . liftM (\x -> (Right x, zero))

instance (MonadBaseControl b m, Monoid e, Plus f) => MonadBaseControl b (ContinueT e f m) where
    data StM (ContinueT e f m) a =
        StContinueT (StM m (Either e a, f (ContinueT e f m a)))

    liftBaseWith k =
        ContinueT $ do
            x <-
                liftBaseWith $ \runB ->
                    k $ \(ContinueT c) ->
                        liftM StContinueT (runB c)
            return (Right x, zero)

    restoreM (StContinueT s) = ContinueT (restoreM s)


-- | 'ContinueT' over 'Identity'.

type Continue e f = ContinueT e f Identity


-- | Type alias for the common case of using @'Last' 'SomeException'@ as
-- the suspension monoid.

type LastEx = Last SomeException
