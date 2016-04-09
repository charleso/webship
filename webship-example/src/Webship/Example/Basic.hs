{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Webship.Example.Basic where

import           Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.State (StateT, evalStateT, get)

import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack)
import           Data.Time.Clock

import qualified Network.HTTP.Types as HTTP
import           Network.Wai (Application, Request)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import           P
import qualified Prelude as Unsafe (error)

import           Portmanteau.Core
import           Portmanteau.Lens

import           System.IO (IO, putStrLn)

import           Webship.Http
import           Webship.Path
import           Webship.Resource
import           Webship.Route
import           Webship.Route.Wai
import           Webship.Wai


newtype State =
  State {
      _getState :: MVar (HashMap AccountName Integer)
    }

-- | Represents the different types of incoming/outgoing content that this example supports
data Plain = Plain

newtype AccountName =
  AccountName {
      renderAccountName :: Text
    } deriving (Eq, Hashable, Ord, Show)
makeIso ''AccountName


resourceWithBody :: MonadIO m => Text -> Resource (StateT State m)
resourceWithBody t =
  resource
    resourceDesc { contentTypesProvided = ("text/plain", Plain) :| [] } .
    return $ Right $ \_ accept -> do
      now <- liftIO getCurrentTime
      return $ ResourceExists (CacheData (Just now) (Just $ Strong "abc123")) $ \case
        ROther -> case accept of
          Plain -> return . ok . ResponseBuilder . fromHtmlEscapedText $ t
        _ -> serverError

accountResource :: MonadIO m => AccountName -> Resource (StateT State m)
accountResource accountName req =
  let rd = resourceDesc {
        allowedMethods = [HTTP.methodGet, HTTP.methodHead, HTTP.methodPost, HTTP.methodPut]
      , contentTypesAccepted = ("text/plain", Plain) :| []
      , contentTypesProvided = ("text/plain", Plain) :| []
      }
      resource' a = resource rd a req
  in resource' . return . Right $ \contentType accept -> do
    s <- get
    m <- liftIO (readMVar (_getState s))
    now <- liftIO getCurrentTime
    let cache = noCacheData { cacheModified = Just now }
    return $ case HM.lookup accountName m of
      Nothing ->
        ResourceNotFound $ \case
          MPost ->
            return MPostNotFound
          MOther ->
            return MOtherNotFound
          MPut -> do
            val <- liftIO $ case contentType of
              Plain -> readBody req
            liftIO (modifyMVar_ (_getState s) (return . HM.insert accountName val))
            return . MPutOk cache . ok $ Empty
      Just val ->
        ResourceExists cache $ \case
          ROther -> case accept of
            Plain ->
              return . ok $ ResponseBuilder (fromHtmlEscapedText (pack (show val) <> "\n"))
          RPut -> do
            val' <- liftIO $ case contentType of
              Plain -> readBody req
            liftIO (modifyMVar_ (_getState s) (return . HM.insert accountName val'))
            return . RPutOk . ok $ Empty
          -- POST'ing to this resource adds the integer to the current value
          RPost -> do
            val' <- liftIO $ case contentType of
              Plain -> readBody req
            liftIO (modifyMVar_ (_getState s) (return . HM.insertWith (+) accountName val'))
            return . RPostOk . ok $ Empty
          RDelete ->
            serverError

resource404 :: Monad m => Resource m
resource404 =
  resource
    resourceDesc { contentTypesProvided = ("text/html", ()) :| [] } .
    return $ Right $ \_ _ ->
      return $ ResourceNotFound $ \case
        MOther ->
          return MOtherNotFound
        _ ->
          serverError

myRoutes :: RoutingSpec (Resource (StateT State IO)) ()
myRoutes = do
  root @>
    resourceWithBody "Just the root resource"
  seg "account" *| (_AccountName |$| var) #>
    accountResource

main :: IO ()
main = do
  putStrLn "Listening on port 3000"
  Warp.runSettings
    (Warp.setPort 3000 (Warp.setHost "127.0.0.1" Warp.defaultSettings))
    =<< basicApplication

basicApplication :: IO Application
basicApplication = do
  s <- State <$> newMVar HM.empty
  return $ routesToWai (resourceToWaiT (\_ -> flip evalStateT s)) myRoutes resource404


readBody :: Request -> IO Integer
readBody =
  fmap (fromMaybe 0 . readMaybe . LB.unpack) . Wai.strictRequestBody

serverError :: a
serverError =
  Unsafe.error "error: There should be an EitherT in this example"
