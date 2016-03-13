{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Webship.Decision.Data (
    CacheData (..)
  , IfMatch (..)
  , IfNoneMatch (..)
  , IfModifiedSinceHeader (..)
  , IfModifiedSince (..)
  , IfUnmodifiedSinceHeader (..)
  , IfUnmodifiedSince (..)
  , AcceptHeader (..)
  , AcceptLanguage (..)
  , AcceptCharset (..)
  , AcceptEncoding (..)
  , Location (..)
  , Available (..)
  , PreviouslyExisted (..)
  , MultipleRepresentations (..)
  , Moved (..)
  , ResourceDenied (..)
  , noCacheData
  ) where

import           Data.ByteString (ByteString)
import           Data.Time (UTCTime)

import           Webship.Http (ETag (..))


-- FIX Move some of this to webship-http?

data CacheData =
  CacheData {
      cacheModified :: Maybe UTCTime
    , cacheETag :: Maybe ETag
    } deriving (Eq, Show)

newtype IfMatch =
  IfMatch {
      renderIfMatch :: ByteString
    } deriving (Eq, Show)

newtype IfNoneMatch =
  IfNoneMatch {
      renderIfNoneMatch :: ByteString
    } deriving (Eq, Show)

newtype IfModifiedSinceHeader =
  IfModifiedSinceHeader ByteString

newtype IfModifiedSince =
  IfModifiedSince UTCTime

newtype IfUnmodifiedSinceHeader =
  IfUnmodifiedSinceHeader ByteString

newtype IfUnmodifiedSince =
  IfUnmodifiedSince UTCTime

newtype AcceptHeader =
  AcceptHeader ByteString

newtype AcceptLanguage =
  AcceptLanguage ByteString

newtype AcceptCharset =
  AcceptCharset {
      renderAcceptCharset :: ByteString
    } deriving (Eq, Show)

newtype AcceptEncoding =
  AcceptEncoding {
      renderAcceptEncoding :: ByteString
    } deriving (Eq, Show)

newtype Location =
  Location {
      renderLocation :: ByteString
    } deriving (Eq, Show)


data PreviouslyExisted =
    PreviouslyExisted
  | NeverSeenBefore
  deriving (Eq, Show)

data Available =
    Available
  | Unavailable
  deriving (Eq, Show)

data MultipleRepresentations =
    MultipleRepresentations
  | OK
  deriving (Eq, Show)

data Moved =
    MovedPermanently Location
  | MovedTemporarily Location
  deriving (Eq, Show)

data ResourceDenied =
    Forbidden
  | Unauthorized
  | MalformedRequest
  | UnsupportedContentType
  | UriTooLong
  | EntityTooLarge
  deriving (Eq, Show)

noCacheData :: CacheData
noCacheData =
  CacheData Nothing Nothing
