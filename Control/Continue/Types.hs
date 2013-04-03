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
      Continue(..),

      -- * Convenience types
      LastException
    )
    where

import qualified Data.Bifunctor as Bi
import Control.Applicative
import Control.Arrow
import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Functor.Plus
import Data.Monoid


-- | This monad transformer adds suspensions under @f@ to @m@.

newtype Continue e f m a =
    Continue {
      runContinue :: m (Either e a, f (Continue e f m a))
    }

instance (Monad m, Monoid e, Plus f) => Alternative (Continue e f m) where
    empty = Continue (return (Left mempty, zero))

    Continue c1 <|> Continue c2 =
        Continue $ do
            (mx1, cf1) <- c1
            (mx2, cf2) <- c2
            let mx = either (\ex1 -> Bi.first (ex1 <>) mx2) Right mx1
            return (mx, cf1 <!> cf2)

instance (Monad m, Plus f) => Applicative (Continue e f m) where
    pure x = Continue (return (Right x, zero))

    Continue cf <*> rx@(Continue cx) =
        Continue $ do
            (mf, cff) <- cf
            case mf of
              Left ex -> return (Left ex, fmap (<*> rx) cff)
              Right f -> do
                  (mx, cfx) <- cx
                  return (fmap f mx, fmap (fmap f) cfx)

instance (Functor f, Monad m) => Functor (Continue e f m) where
    fmap f (Continue c) = Continue (liftM (fmap f *** fmap (fmap f)) c)

instance (Monad m, Monoid e, Plus f) => Monad (Continue e f m) where
    return = pure

    Continue c >>= f =
        Continue $ do
            (mx, cfx') <- c
            let cfx = fmap (>>= f) cfx'
            case mx of
              Left ex -> return (Left ex, cfx)
              Right x -> do
                  (my, cfy) <- runContinue (f x)
                  return (my, cfx <!> cfy)

    fail _ = Continue (return (Left mempty, zero))

instance (MonadBase b m, Monoid e, Plus f) => MonadBase b (Continue e f m) where
    liftBase = liftBaseDefault

instance (MonadFix m, Monoid e, Plus f) => MonadFix (Continue e f m) where
    mfix f =
        Continue . mfix $ \ ~(mx, _) ->
            runContinue . f .
            either (const $ error "Feedback broken by suspension") id $ mx

instance (MonadIO m, Monoid e, Plus f) => MonadIO (Continue e f m) where
    liftIO = Continue . liftM (\x -> (Right x, zero)) . liftIO

instance (Monad m, Monoid e, Plus f) => MonadPlus (Continue e f m) where
    mzero = empty
    mplus = (<|>)

instance (Plus f) => MonadTrans (Continue e f) where
    lift = Continue . liftM (\x -> (Right x, zero))

instance (MonadBaseControl b m, Monoid e, Plus f) => MonadBaseControl b (Continue e f m) where
    data StM (Continue e f m) a =
        ContinueStM (StM m (Either e a, f (Continue e f m a)))

    liftBaseWith k =
        Continue $ do
            x <-
                liftBaseWith $ \runB ->
                    k $ \(Continue c) ->
                        liftM ContinueStM (runB c)
            return (Right x, zero)

    restoreM (ContinueStM s) = Continue (restoreM s)


-- | Type alias for the common case of using @'Last' 'SomeException'@ as
-- the suspension monoid.

type LastException = Last SomeException
