{-# LANGUAGE NoImplicitPrelude #-}
module Webship.Wai.Response (
    ResponseBody (..)
  , toWaiResponse
  ) where

import           Blaze.ByteString.Builder (Builder)

import           Network.HTTP.Types (Header, Status)
import qualified Network.Wai as Wai

import           P

import           System.IO (FilePath)


-- | Basically Wai's unexported 'Response' type.
data ResponseBody =
   ResponseFile FilePath (Maybe Wai.FilePart)
 | ResponseBuilder Builder
 | ResponseStream Wai.StreamingBody
 | Empty
 -- ResponseRaw ... (not implemented yet, but useful for websocket upgrades)


toWaiResponse :: Status -> [Header] -> ResponseBody -> Wai.Response
toWaiResponse s h body =
  case body of
    ResponseBuilder b ->
      Wai.responseBuilder s h b
    ResponseFile path part ->
      Wai.responseFile s h path part
    ResponseStream streamer ->
      Wai.responseStream s h streamer
    Empty ->
      Wai.responseBuilder s h mempty
