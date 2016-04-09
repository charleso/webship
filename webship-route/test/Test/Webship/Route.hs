{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Webship.Route where

import           Data.Text (Text)

import           P

import           Portmanteau.Core

import           System.IO (IO)

import           Test.QuickCheck ((===), conjoin, quickCheckAll)

import           Webship.Path
import           Webship.Route


prop_matchRouteRoot =
  conjoin [
      matchRoute' [] root === Just ()
    , matchRoute' ["a"] root === Nothing
    ]

prop_matchRouteStatic =
  conjoin [
      matchRoute' ["a", "b", "c", "x"] (root |* seg "a" |* seg "b" |* seg "c" |* seg "d") === Nothing
    , matchRoute' ["a", "b", "c", "d"] (root |* seg "a" |* seg "b" |* seg "c" |* seg "d") === Just ()
    ]

prop_matchRouteVar =
  conjoin [
      matchRoute' ["a", "b", "c", "x"] (seg "a" *| var |*| var |* seg "d") === Nothing
    , matchRoute' ["a", "b", "c"] (seg "a" *| var |*| var |* seg "d") === Nothing
    , matchRoute' ["a", "b", "c", "d"] (seg "a" *| var |*| var |* seg "d") === Just ("b", "c")
    , matchRoute' ["a", "b", "c", "d"] (seg "a" *| var |*| seg "c" *| var) === Just ("b", "d")
    ]

-- FIX Because I'm too lazy to rewrite the tests
matchRoute' :: [Text] -> Path' e a -> Maybe a
matchRoute' s p =
  route (runRouter $ p #> id) s


return []
tests :: IO Bool
tests = $quickCheckAll
