{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Webship.Path where

import           P
import           Portmanteau.Core

import           System.IO (IO)

import           Test.QuickCheck (Property, (===), quickCheckAll)
import           Test.QuickCheck.Instances ()

import           Webship.Path


prop_path_example_1 a =
  pathTripping a $
    seg "a" *| var |*| seg "b" *| pathInt var


pathTripping :: (Eq a, Show a) => a -> Path a -> Property
pathTripping a p =
  decodePath p (encodePath p a) === pure a


return []
tests :: IO Bool
tests = $quickCheckAll
