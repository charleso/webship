{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Webship.Http.ETag (
    ETag (..)
  , parseEtagList
  , etagToByteString
  ) where

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import           Data.ByteString (ByteString)

import           P


data ETag =
    Strong ByteString
  | Weak ByteString
  deriving (Eq, Ord, Show)


etagToByteString :: ETag -> ByteString
etagToByteString = \case
  Strong bs ->
    "\"" <> bs <> "\""
  Weak bs ->
    "W/\"" <> bs <> "\""

weakETag :: Parser ETag
weakETag =
  Weak <$> (AP.string "W/" *> insideQuotes rest)
  where
    rest = AP.takeWhile1 (/= doubleQuote)

strongETag :: Parser ETag
strongETag =
  insideQuotes strong
  where
    strong = Strong <$> AP.takeWhile1 (/= doubleQuote)

eTag :: Parser ETag
eTag =
  insideWhitespace (weakETag <|> strongETag)

-- | Parse a list of Etags, returning an empty list if parsing fails
parseEtagList :: ByteString -> [ETag]
parseEtagList input =
  either (const []) id parseResult
  where
    parseResult = AP.parseOnly eTagList input
    eTagList = AP.sepBy' eTag comma <* AP.endOfInput

comma :: Parser Char
comma =
  AP.char ','

doubleQuote :: Char
doubleQuote =
  '"'

insideQuotes :: Parser a -> Parser a
insideQuotes a =
  AP.char doubleQuote *> a <* AP.char doubleQuote

optionalWhitespace :: Parser ByteString
optionalWhitespace =
  AP.takeWhile $ AP.inClass " \t"

insideWhitespace :: Parser a -> Parser a
insideWhitespace a =
  optionalWhitespace *> a <* optionalWhitespace

