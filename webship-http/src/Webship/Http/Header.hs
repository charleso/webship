{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Webship.Http.Header (
    hAcceptCharset
  , hAcceptEncoding
  , hIfUnmodifiedSince
  , hIfMatch
  , hIfNoneMatch
  ) where

import           Network.HTTP.Types (HeaderName)


-- TODO this exist in http-types-0.9
hAcceptCharset :: HeaderName
hAcceptCharset =
  "Accept-Charset"

hAcceptEncoding :: HeaderName
hAcceptEncoding =
  "Accept-Encoding"

hIfMatch :: HeaderName
hIfMatch =
  "If-Match"

hIfUnmodifiedSince :: HeaderName
hIfUnmodifiedSince =
  "If-Unmodified-Since"

hIfNoneMatch :: HeaderName
hIfNoneMatch =
  "If-None-Match"
