{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Webship.Path (
    Path'
  , Path
  , PathDecoder
  , PathError (..)
  , encodePath
  , decodePath
  , decodePath'
  , pathDecoder
  , root
  , var
  , seg
  , varParse
  , varParse'
  , pathInt
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT (..))

import           Data.Functor.Contravariant (Op (..))
import           Data.Text (Text)
import qualified Data.Text as T

import           P

import           Portmanteau.Core (Codec' (..), Codec, (|>|))


{- |
@
data Blog = Blog Text Text
makeIso ''Blog

_Blog
  '|$|' 'seg' "blog"
   '*|' 'var'
  '|*|' 'var'
@
-}
type Path' e a = Codec (Op [Text]) (PathDecoder e) a
type Path a = Path' Text a
type PathDecoder e = StateT [Text] (Either (PathError e))

data PathError e =
    PathConsumed
  | PathPartiallyConsumed [Text]
  | PathInvalid Text Text
  | PathParseError e
  deriving (Eq, Show)


encodePath :: Path' e a -> a -> [Text]
encodePath (Codec (Op e) _) a =
  e a

decodePath :: Path' e a -> [Text] -> Either (PathError e) a
decodePath (Codec _ d) =
  decodePath' d

decodePath' :: PathDecoder e a -> [Text] -> Either (PathError e) a
decodePath' d s =
  runStateT d s >>= \(a, l) ->
    if null l then pure a else Left (PathPartiallyConsumed l)

pathDecoder :: Path' e a -> PathDecoder e a
pathDecoder =
  codecDecoder


root :: Path' e ()
root =
  Codec (Op $ \() -> []) (pure ())

var :: Path' e Text
var =
  varParse id pure

seg :: Text -> Path' e ()
seg n =
  varParse'
    (\() -> n)
    (\t -> if t == n then Right () else Left $ PathInvalid t n)

varParse :: (a -> Text) -> (Text -> Either e a) -> Path' e a
varParse e d =
  varParse' e (first PathParseError . d)

varParse' :: (a -> Text) -> (Text -> Either (PathError e) a) -> Path' e a
varParse' e d =
  Codec
    (Op (pure . e))
    (StateT $ \xs -> case xs of [] -> Left PathConsumed; h : t -> flip (,) t <$> d h)

pathInt :: Path Text -> Path Int
pathInt =
  T.pack . show |>| lift . first (PathParseError . T.pack) . readEither . T.unpack
