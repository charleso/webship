{-# LANGUAGE NoImplicitPrelude #-}
module Webship.Http.Date (
    parseRfc1123Date
  , utcTimeToRfc1123
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import           Data.Time.Format (formatTime)

import qualified Network.HTTP.Date as HD

import           P

import           System.Locale (defaultTimeLocale)


parseRfc1123Date :: ByteString -> Maybe UTCTime
parseRfc1123Date b =
  httpDateToUtc <$> HD.parseHTTPDate b

utcTimeToRfc1123 :: UTCTime -> ByteString
utcTimeToRfc1123 utc =
  C8.pack $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" utc

httpDateToUtc :: HD.HTTPDate -> UTCTime
httpDateToUtc h =
  UTCTime days diffTime
    where
      days = fromGregorian (fromIntegral $ HD.hdYear h) (HD.hdMonth h) (HD.hdDay h)
      diffTime = secondsToDiffTime seconds
      seconds = fromIntegral $ hourS + minS + HD.hdSecond h
      hourS = HD.hdHour h * 60 * 60
      minS = HD.hdMinute h * 60

