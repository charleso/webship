{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Webship.Route (
    (</>)
  , (/>)
  , (</)
  , (#>)
  , (@>)
  , Route
  , RoutingSpec
  , star
  , root
  , var
  , runRouter
  , route
  , matchRoute
  , matchRoute'
  ) where

import           Control.Monad.Trans.State (State, evalState, modify, put, state)
import           Control.Monad.Trans.Writer (Writer, WriterT (..), execWriter, tell)

import           Data.String (IsString, fromString)
import           Data.Text (Text)

import           P
import qualified Prelude as Unsafe (head)


infixl 4 </>
infixl 5 />
infixl 5 </
infixl 3 #>
infixl 3 @>

-- | 'Route's represent chunks of text used to match over URLs.
-- You match hardcoded paths with string literals (and the @-XOverloadedStrings@ extension),
-- captured variables with the 'var' combinator, and wildcards with 'star'.
data Route a =
  Route {
      getRoute :: [BoundOrUnbound]
    -- Allow each 'Route' to consume segments and convert to a value
    , routeValue :: State [Text] a
    } deriving (Functor)

instance Applicative Route where
  pure a =
    Route [] (return a)
  (<*>) (Route r1 v1) (Route r2 v2) =
    Route (r1 <> r2) (v1 <*> v2)

data BoundOrUnbound =
    Bound Text
  | Var
  | RestUnbound deriving (Show)

instance IsString (Route ()) where
    fromString s = Route [Bound (fromString s)] (modify (drop 1))

data RouteResource m =
  RouteResource (Route m)


runRouter :: RoutingSpec m a -> [RouteResource m]
runRouter routes =
  execWriter (getRouter routes)

-- | @a '</>' b@ separates the path components @a@ and @b@ with a slash.
(</>) :: Route (a -> b) -> Route a -> Route b
(</>) = (<*>)

-- | @a '/>' b@ separates the path components @a@ and @b@ with a slash, discarding the value of the first argument
(/>) :: Route () -> Route b -> Route b
(/>) = (*>)

-- | @a '</' b@ separates the path components @a@ and @b@ with a slash, discarding the value of the second argument
(</) :: Route a -> Route () -> Route a
(</) = (<*)

-- | Represents the root resource (@/@). This should usually be the first path declared in a 'RoutingSpec'.
root :: Route ()
root =
  Route [] (return ())

-- | Captures a named in a route and adds it to the 'routingParams' hashmap under the provided 'Text' value. For example,
--
-- @
--    "blog" '/>' 'var' '</>' 'var'
-- @
--
-- will capture all URLs of the form @\/blog\/$date\/$post@, and add @date@ and @post@ to the 'routingParams'
-- contained within the resource this route maps to.
var :: Route Text
var =
  Route [Var]
    -- Yuck. :( Note that given the implementation of 'matchesRoute' it should be impossible to fail
    (state (\t -> (Unsafe.head t, drop 1 t)))

-- | Captures a wildcard route. For example,
--
-- @
--    "emcees" '</>' star
-- @
--
-- will match @\/emcees@, @\/emcees/biggie@, @\/emcees\/earl\/vince@, and so on and so forth.
star :: Route ()
star =
  Route [RestUnbound] (put [] >> return ())

-- | Represents a fully-specified set of routes that map paths (represented as 'Route's) to 'Resource's. 'RoutingSpec's are declared with do-notation, to wit:
--
-- @
--    myRoutes :: RoutingSpec IO ()
--    myRoutes = do
--      root                                 @> myRootResource
--      Blog <$> "blog" '/>' var '</>' var   #> blogPostResource
--      "about"                              @> aboutResource
--      "anything" '/>' star                 @> wildcardResource
-- @
--
newtype RoutingSpec m a =
  RoutingSpec {
      getRouter :: Writer [RouteResource m] a
    } deriving (Functor, Applicative, Monad)

-- Indicate that a particular 'Route' maps to a given 'Resource'
(#>) :: Route p -> (p -> r) -> RoutingSpec r ()
k #> v =
  RoutingSpec $ tell [RouteResource $ fmap v k]

(@>) :: Route () -> r -> RoutingSpec r ()
k @> v =
  k #> (const v)

route :: [RouteResource r] -> [Text] -> r -> (r, [Text])
route routes pInfo resource404 =
  foldr' (matchRoute pInfo) (resource404, mempty) routes

matchRoute :: [Text] -> RouteResource r -> (r, [Text]) -> (r, [Text])
matchRoute paths (RouteResource rSpec) (previousMatch, previousMap) =
  case matchRoute' paths rSpec of
    Nothing -> (previousMatch, previousMap)
    Just (resource, s) -> (resource, s)

matchRoute' :: [Text] -> Route a -> Maybe (a, [Text])
matchRoute' paths rSpec =
  matchesRoute paths rSpec >>= \s ->
    return (evalState (routeValue rSpec) paths, s)

matchesRoute :: [Text] -> Route p -> Maybe [Text]
matchesRoute paths spec =
  matchesRoute' paths (getRoute spec) mempty False
  where
    -- recursion is over, and we never bailed out to return false, so we match
    matchesRoute' []        []              acc     _   = Just acc
    -- there is an extra part of the path left, and we don't have more matching
    matchesRoute' (_ph:_ptl) []             _       _   = Nothing
    -- we match whatever is left, so it doesn't matter what's left in the path
    matchesRoute' r        (RestUnbound:_) d        _   = Just (d <> r)
    -- we match a specific string, and it matches this part of the path,
    -- so recur
    matchesRoute' (ph:ptl)  (Bound sh:stt)  dispatch True
        | ph == sh
                                                    = matchesRoute' ptl stt (dispatch <> [ph]) True
    matchesRoute' (ph:ptl)  (Bound sh:stt)  dispatch False
        | ph == sh
                                                    = matchesRoute' ptl stt dispatch False
    matchesRoute' (_:ptl)   (Var:stt)       acc   _ = matchesRoute' ptl stt acc True
    matchesRoute' _         _               _acc  _ = Nothing
