{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Webship.Route where

import           P

import           System.IO (IO)

import           Test.QuickCheck ((===), conjoin, quickCheckAll)

import           Webship.Route


prop_matchRouteRoot =
  conjoin [
      matchRoute' [] root === Just ((), [])
    , matchRoute' ["a"] root === Nothing
    ]

prop_matchRouteStatic =
  conjoin [
      matchRoute' ["a", "b", "c", "x"] (root </ "a" </ "b" </ "c" </ "d") === Nothing
    , matchRoute' ["a", "b", "c", "d"] (root </ "a" </ "b" </ "c" </ "d") === Just ((), [])
    ]

prop_matchRouteVar =
  conjoin [
      matchRoute' ["a", "b", "c", "x"] ((,) <$> "a" /> var </> var </ "d") === Nothing
    , matchRoute' ["a", "b", "c"] ((,) <$> "a" /> var </> var </ "d") === Nothing
    , matchRoute' ["a", "b", "c", "d"] ((,) <$> "a" /> var </> var </ "d") === Just (("b", "c"), ["d"])
    , matchRoute' ["a", "b", "c", "d"] ((,) <$> "a" /> var </> "c" /> var) === Just (("b", "d"), ["c"])
    , matchRoute' ["a", "b", "c", "d"] ((,) <$> "a" /> var </> ((,) <$> var </> var)) === Just (("b", ("c", "d")), [])
    ]

prop_matchRouteStar =
  conjoin [
      matchRoute' ["a", "b", "c", "d"] star === Just ((), ["a", "b", "c", "d"])
    , matchRoute' ["a", "b", "c", "d"] ("a" /> star) === Just ((), ["b", "c", "d"])
    ]


return []
tests :: IO Bool
tests = $quickCheckAll
