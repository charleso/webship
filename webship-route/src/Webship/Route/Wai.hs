{-# LANGUAGE NoImplicitPrelude #-}
module Webship.Route.Wai (
    routesToWai
  ) where

import           Network.Wai (Application)
import qualified Network.Wai as Wai

import           P

import           Webship.Route


routesToWai :: (m -> Application) -> RoutingSpec m () -> m -> Application
routesToWai run routes resource404 req respond =
  run (fromMaybe resource404 $ route (runRouter routes) (Wai.pathInfo req)) req respond
