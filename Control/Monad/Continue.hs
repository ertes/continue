-- |
-- Module:     Control.Monad.Continue
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- This library implements a monad transformer for suspendable
-- computations, similar and related to free comonads.  It allows to
-- write continuation-based web frameworks, command line applications
-- and similar interfaces, where you want to reenter a computation at
-- arbitrary spots.

{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Continue
    ( -- $doc

      -- * Continue
      Continue,
      runContinue,

      -- * ContinueT
      ContinueT(..),
      mapContinueT,

      -- * Combinators
      orElse,

      -- * Convenience types
      LastEx,

      -- * Reexports
      module Control.Monad.Continue.Class,
      Alt(..),
      Plus(..)
    )
    where

import qualified Data.Bifunctor as Bi
import Control.Applicative
import Control.Arrow
import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Continue.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import Data.Functor.Plus
import Data.Monoid


-- | This monad transformer adds continuations under @f@ and @e@-typed
-- suspensions to @m@.

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

instance (Monad m, Monoid e, Plus f) => MonadContinue e f (ContinueT e f m) where
    addCont mx cf = ContinueT (return (mx, cf))

instance (Monad m, Monoid e, Plus f) => MonadError e (ContinueT e f m) where
    throwError ex = ContinueT (return (Left ex, zero))
    catchError (ContinueT c) h =
        ContinueT $ do
            (mx, cf) <- c
            case mx of
              Left ex -> do
                  (mxh, cfh) <- runContinueT (h ex)
                  return (mxh, cf <!> cfh)
              Right _ -> return (mx, cf)

-- | Warning: If feedback is broken by suspension you get a run-time
-- error.

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

instance (MonadReader r m, Monoid e, Plus f) => MonadReader r (ContinueT e f m) where
    ask = lift ask
    local f (ContinueT c) = ContinueT (local f c)
    reader = ContinueT . liftM (\x -> (Right x, zero)) . reader

instance (MonadState s m, Monoid e, Plus f) => MonadState s (ContinueT e f m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Plus f) => MonadTrans (ContinueT e f) where
    lift = ContinueT . liftM (\x -> (Right x, zero))

instance (MonadWriter l m, Monoid e, Plus f) => MonadWriter l (ContinueT e f m) where
    listen (ContinueT c) =
        ContinueT $ do
            ((mx, cf), w) <- listen c
            let addLog x = (x, w)
            return (fmap addLog mx, fmap (fmap addLog) cf)

    pass (ContinueT c) =
        ContinueT . pass $ do
            (mx, cf') <- c
            let cf = fmap (fmap fst) cf'
            case mx of
              Left ex      -> return ((Left ex, cf), id)
              Right (x, f) -> return ((Right x, cf), f)

    tell = lift . tell
    writer = lift . writer


-- | 'ContinueT' over 'Identity'.

type Continue e f = ContinueT e f Identity


-- | Type alias for the common case of using @'Last' 'SomeException'@ as
-- the suspension monoid.

type LastEx = Last SomeException


-- | Apply the given morphism to the underlying monad.

mapContinueT ::
    (Functor f, Monad n)
    => (forall a. m a -> n a)  -- ^ Monad morphism to apply.
    -> ContinueT e f m a
    -> ContinueT e f n a
mapContinueT mm (ContinueT c) =
    ContinueT $ do
        (mx, cf) <- mm c
        return (mx, fmap (mapContinueT mm) cf)


-- | Similar to '<|>', but tries the second computation only if the
-- first one actually suspends.  Note that not running the second
-- computation also means that it can't register reentry spots.
--
-- As an operator this function is infixr 3.

orElse ::
    (Alt f, Monad m, Monoid e)
    => ContinueT e f m a
    -> ContinueT e f m a
    -> ContinueT e f m a
orElse (ContinueT c1) (ContinueT c2) =
    ContinueT $ do
        (mx1, cf1) <- c1
        case mx1 of
          Left ex1 -> do
              (mx2, cf2) <- c2
              return (Bi.first (ex1 <>) mx2, cf1 <!> cf2)
          Right _ -> return (mx1, cf1)

infixr 3 `orElse`


-- | Run the given 'Continue' computation.

runContinue :: Continue e f a -> (Either e a, f (Continue e f a))
runContinue = runIdentity . runContinueT


{- $doc

A computation of type @'ContinueT' e f m a@ is a computation that may
either conclude with a value of type @a@ or suspend with a value of type
@e@.  Before suspending or concluding it may register a set of reentry
spots of type @f (ContinueT e f m a)@.  These spots are collected and
returned along with the suspension/conclusion value:

> newtype ContinueT e f m a

To run a @ContinueT@ computation you can use the 'runContinueT'
function:

> runContinueT :: ContinueT e f m a
>              -> m (Either e a, f (ContinueT e f m a))

The result is either a suspension value of type @e@ or a conclusion of
type @a@.  In both cases you can reenter the computation at the
registered spots.  Example:

> type MyMonad = ContinueT () (Map String) Identity
>
> myComp :: MyMonad Integer
> myComp = do
>     x <- continue (Right 3) (M.singleton "x" (Right 15))
>     y <- continue (Right 4) (M.singleton "y" (Right 17))
>     return (x + y)

When you first run this computation the result will be the conclusion 3
+ 4.  Since 'MyMonad' transforms to 'Identity' we can use the
convenience type alias 'Continue' and the function 'runContinue':

> type MyMonad = Continue () (Map String)
>
> runContinue myComp
> = (Right 7, reentryMap)

Along with the result you will also get a reentry map of type @Map
String (MyMonad Integer)@.  If you run the computation indexed by \"x\",
you will get the result 15 + 4.  This iteration itself will return a new
reentry map on its part.  That map will contain only the reentry spot
\"y\", because the reentry indexed by \"x\" does not register itself
again.

You can use the more general 'addCont' function to register arbitrary
reentry spots, which themselves are allowed to register new spots.  Also
In some kinds of applications you would want to combine the reentry maps
produced.  You can use the '<!>' function to do that:

> let overallReentryMap = reentryMap1 <!> reentryMap2

Since @e@ is required to be a monoid, @ContinueT@ forms a family of
alternative functors that implement choice based on suspension and
conclusion.  The computation 'empty' always suspends with 'mempty'.

> x <|> y

This computation concludes with the conclusion of either @x@ or @y@
trying them in that order, or suspends if both of them suspend.  Note
that both computations are performed to their conclusion or suspension.
This allows @y@ both to have monadic effects as well as to register
reentry points, even if @x@ concludes.

There is also a combinator 'orElse' that tries @y@ only if @x@ actually
suspends.  In that case, if @x@ concludes, then @y@ cannot register
reentry spots or have effects in the underlying monad.

-}
