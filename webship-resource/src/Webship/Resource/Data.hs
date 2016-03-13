{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Webship.Resource.Data (
    MOtherAction (..)
  , MPostAction (..)
  , MPutAction (..)
  , Moved (..)
  , MultipleRepresentations (..)
  , OctetStream (..)
  , ResourceDenied (..)
  , ResourceDesc (..)
  , ResponseEntity (..)
  , ResourceExists (..)
  , ResourceMethod (..)
  , ResourceNotFoundMethod (..)
  , RDeleteAction (..)
  , RPostAction (..)
  , RPutAction (..)
  , ok
  , resourceDesc
  ) where

import           Data.List.NonEmpty (NonEmpty (..))

import           Network.HTTP.Media (MediaType)
import           Network.HTTP.Types (Method)
import qualified Network.HTTP.Types as HTTP

import           Webship.Decision.Data
import           Webship.Wai


data ResourceDesc a b =
  ResourceDesc {
    serviceAvailable :: Available
  , allowedMethods :: [Method]
  , contentTypesAccepted :: NonEmpty (MediaType, a)
  , contentTypesProvided :: NonEmpty (MediaType, b)
  , languageAvailable :: AcceptLanguage -> Bool
  }

data ResponseEntity =
    ResponseEntity MultipleRepresentations ResponseBody



data ResourceExists m =
    ResourceExists CacheData (forall a. ResourceMethod a -> m a)
  | ResourceNotFound (forall a. ResourceNotFoundMethod a -> m a)

data ResourceMethod a where
  RPut :: ResourceMethod RPutAction
  RPost :: ResourceMethod RPostAction
  RDelete :: ResourceMethod RDeleteAction
  ROther :: ResourceMethod ResponseEntity

data ResourceNotFoundMethod a where
  MPut :: ResourceNotFoundMethod MPutAction
  MPost :: ResourceNotFoundMethod MPostAction
  MOther :: ResourceNotFoundMethod MOtherAction

data RPutAction =
    RPutConflict
  | RPutCreated Location
  | RPutOk ResponseEntity

data RPostAction =
    RPostSeeOther Location
  | RPostCreated Location
  | RPostOk ResponseEntity

data RDeleteAction =
    RDeleteAccepted
  | RDeleteOk ResponseEntity

data MPutAction =
    MPutMoved Location
  | MPutConflict
  | MPutCreated Location
  | MPutOk CacheData ResponseEntity

data MPostAction =
    MPostMoved Moved
  | MPostGone
  | MPostNotFound
  | MPostSeeOther Location
  | MPostRedirect Location
  | MPostOk CacheData ResponseEntity

data MOtherAction =
    MOtherMoved Moved
  | MOtherGone
  | MOtherNotFound


data OctetStream = OctetStream deriving (Eq, Show)


resourceDesc :: ResourceDesc OctetStream OctetStream
resourceDesc =
    ResourceDesc
        Available
        [HTTP.methodGet, HTTP.methodHead]
        (("application/octet-stream", OctetStream) :| [])
        (("application/octet-stream", OctetStream) :| [])
        (const True)

ok :: ResponseBody -> ResponseEntity
ok =
    ResponseEntity OK
