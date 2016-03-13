{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Webship.Resource.Wai (
    resourceToWai
  , resourceToWaiT
  ) where

import           Data.Time (getCurrentTime)

import           Control.Monad.Trans.Either (EitherT (..))
import           Control.Monad.Trans.Writer (WriterT (..))

import qualified Network.HTTP.Types as HTTP
import           Network.Wai (Application, Request, Response)

import           P

import           System.IO (IO)

import           Webship.Decision (PartialResponse (..))
import           Webship.Resource.Decision
import           Webship.Wai


resourceToWai :: Resource IO -> Application
resourceToWai =
  resourceToWaiT (const id)

resourceToWaiT :: Functor m => (Request -> m Response -> IO Response) -> Resource m -> Application
resourceToWaiT run fs req respond = do
  now <- getCurrentTime
  -- FIX Add trace headers
  bind respond . run req $ do
    flip fmap (runWriterT . runEitherT . runFlowStateT $ fs req now) $ \(x, (_, PartialResponse h)) ->
      case x of
        Left (b, s) ->
          toWaiResponse s h b
        -- Should be impossible
        Right () ->
          toWaiResponse HTTP.status500 h Empty
