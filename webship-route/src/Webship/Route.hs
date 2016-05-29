{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Webship.Route (
    (#>)
  , (@>)
  , RoutingSpec
  , runRouter
  , route
  ) where

import           Control.Monad.Trans.Writer (Writer, execWriter, tell)

import           Data.Text (Text)

import           P

import           Webship.Path


-- FIX This whole thing should _probably_ just be some form of `|||` on Path

infixl 3 #>
infixl 3 @>


data RouteResource e m =
  RouteResource (PathDecoder e m)


runRouter :: RoutingSpec' e m a -> [RouteResource e m]
runRouter routes =
  execWriter (getRouter routes)


-- | Represents a fully-specified set of routes that map paths (represented as 'Route's) to 'Resource's. 'RoutingSpec's are declared with do-notation, to wit:
--
-- @
--    myRoutes :: RoutingSpec IO ()
--    myRoutes = do
--      root                                     @> myRootResource
--      _Blog |$| seg "blog" '*|' var '|*|' var  #> blogPostResource
--      seg "about"                              @> aboutResource
-- @
--
newtype RoutingSpec' e m a =
  RoutingSpec {
      getRouter :: Writer [RouteResource e m] a
    } deriving (Functor, Applicative, Monad)

type RoutingSpec = RoutingSpec' Text

-- Indicate that a particular 'Path' maps to a given 'Resource'
(#>) :: Path' e p -> (p -> r) -> RoutingSpec' e r ()
k #> v =
  RoutingSpec $ tell [RouteResource . fmap v $ pathDecoder k]

(@>) :: Path' e () -> r -> RoutingSpec' e r ()
k @> v =
  k #> (const v)

route :: [RouteResource e r] -> [Text] -> Maybe r
route routes pInfo =
  -- FIX Throwing away perfectly good errors!
  either Just (const Nothing)
    -- Using Either for stopping on first match
    . mapM (\(RouteResource p) -> either Right Left $ decodePath' p pInfo)
    $ routes
