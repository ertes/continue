-- |
-- Module:     Main
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Test program for the continue package.

module Main where

import qualified Data.Map as M
import Blaze.ByteString.Builder.Char8
import Control.Applicative
import Control.Category
import Control.Concurrent
import Control.Continue
import Control.Monad.Trans
import Data.Conduit
import Data.Map (Map)
import Data.Monoid (mempty)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Prelude hiding ((.), id)
import Text.Boomerang.TH
import Web.Routes
import Web.Routes.Boomerang


data Sitemap
    = AboutR
    | HomeR
    deriving (Eq, Ord)

derivePrinterParsers ''Sitemap

sitemap :: Router () (Sitemap :- ())
sitemap =
    rHomeR <>
    rAboutR . "about"


type App url = ContinueT LastEx (Map url) (ResourceT IO)

instance (MonadContinue e f m) => MonadContinue e f (RouteT url m) where
    addCont mx c =
        RouteT $ \env ->
            addCont mx (fmap (flip unRouteT env) c)


link :: (Ord url) => url -> App url ()
link url =
    continue (Left mempty) (M.singleton url (Right ()))


handle :: App Sitemap Response
handle = home <|> about
    where
    home  = do
        link HomeR
        return (ResponseBuilder ok200 [] (fromString "home"))

    about = do
        link AboutR
        return (ResponseBuilder ok200 [] (fromString "this is a test"))


main :: IO ()
main = do
    contsVar <- newMVar (M.singleton HomeR handle)
    run 8000 $ \req -> do
        let route url = do
                mh <- liftIO $ M.lookup url <$> readMVar contsVar
                case mh of
                  Nothing -> return (ResponseBuilder notFound404 [] (fromString "unavailable"))
                  Just h  -> do
                      (mx, cf) <- lift (runContinueT h)
                      liftIO $ modifyMVar_ contsVar (return . M.union cf)
                      case mx of
                        Left _  -> return (ResponseBuilder notFound404 [] (fromString "suspended"))
                        Right x -> return x

        let site = boomerangSiteRouteT route sitemap

        case runSite "http://localhost:8000/" site (pathInfo req) of
          Left _  -> return (ResponseBuilder notFound404 [] (fromString "not found"))
          Right h -> h
