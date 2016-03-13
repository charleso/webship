{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Webship.Example.Basic where

import qualified Data.ByteString.Lazy as BSL

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WT

import           P

import           System.IO (IO)

import           Test.QuickCheck (Testable, Property, (===), conjoin, quickCheckAll)
import           Test.QuickCheck.Monadic (monadicIO, run, stop)
import           Test.QuickCheck.Instances ()

import           Webship.Example.Basic


prop_example_basic_account a =
  testIO $ do
    app <- basicApplication
    r1 <- makeRequest app Wai.defaultRequest {
        Wai.pathInfo = ["account", a]
      , Wai.requestMethod = HTTP.methodGet
      }
    r2 <- makeRequest app Wai.defaultRequest {
        Wai.pathInfo = ["account", a]
      , Wai.requestMethod = HTTP.methodPut
      , Wai.requestBody = return "1"
      }
    r3 <- makeRequest app Wai.defaultRequest {
        Wai.pathInfo = ["account", a]
      , Wai.requestMethod = HTTP.methodGet
      }
    r4 <- makeRequest app Wai.defaultRequest {
        Wai.pathInfo = ["account", a]
      , Wai.requestMethod = HTTP.methodPost
      , Wai.requestBody = return "1"
      }
    r5 <- makeRequest app Wai.defaultRequest {
        Wai.pathInfo = ["account", a]
      , Wai.requestMethod = HTTP.methodGet
      }
    r6 <- makeRequest app Wai.defaultRequest {
        Wai.pathInfo = ["account", a]
      , Wai.requestMethod = HTTP.methodDelete
      }
    let f r = (WT.simpleStatus r, WT.simpleBody r)
    return $ conjoin [
        f r1 === (HTTP.status404, "")
      , f r2 === (HTTP.status204, "")
      , f r3 === (HTTP.status200, "1\n")
      , f r4 === (HTTP.status204, "")
      , f r5 === (HTTP.status200, "2\n")
      , f r6 === (HTTP.status405, "")
      ]


testIO :: Testable a => IO a -> Property
testIO =
   monadicIO . (=<<) stop . run

makeRequest :: Wai.Application -> Wai.Request -> IO WT.SResponse
makeRequest a r = do
  x <- BSL.fromStrict <$> Wai.requestBody r
  WT.runSession (WT.srequest $ WT.SRequest r x) a


return []
tests :: IO Bool
tests = $quickCheckAll
