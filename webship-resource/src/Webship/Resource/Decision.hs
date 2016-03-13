{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Webship.Resource.Decision (
    Resource
  , FlowStateT (..)
  , resource
  ) where

import           Data.Time (UTCTime)
import qualified Data.List.NonEmpty as NE

import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.Either (EitherT (..), left, mapEitherT)
import           Control.Monad.Trans.Writer (WriterT (..), tell, writer)

import           Network.HTTP.Media (MediaType)
import           Network.HTTP.Types (Status)
import qualified Network.HTTP.Types as HTTP
import           Network.Wai (Request (..))

import           P

import           Webship.Decision (CommittedResponse (..), PartialResponse (..), DecisionName (..), Decision (..), DecisionT, DecisionF, DecisionH)
import           Webship.Decision.Data
import qualified Webship.Decision as D
import           Webship.Resource.Data
import           Webship.Http
import           Webship.Wai


-- FIX type aliases are evil, what do do here?
type Resource m = Request -> UTCTime -> FlowStateT m ()

-- | This is a temporary monad and shouldn't escape this module
newtype FlowStateT m a =
  FlowStateT {
      runFlowStateT :: EitherT (ResponseBody, Status) (WriterT ([DecisionName], PartialResponse) m) a
    } deriving (Monad)

instance MonadTrans FlowStateT where
  lift =
    FlowStateT . EitherT . (>>= return . Right) . lift

resource ::
     Monad m
  => ResourceDesc a b
  -> m (Either ResourceDenied (a -> b -> m (ResourceExists m)))
  -> Resource m
resource rd m req now = do
  (a, (accept, b), re) <- newResource req rd m
  lift (re a b) >>= \case
    ResourceExists c run ->
      resourceExists req now accept c run
    ResourceNotFound run ->
      resourceNotFound req accept run

resourceExists :: Monad m => Request -> UTCTime -> MediaType -> CacheData -> (forall a. ResourceMethod a -> m a) -> FlowStateT m ()
resourceExists req now accept cache f = do
    _ <- g07 (Just ())
    preconditions req now cache
    let
      ok'' a g = (lift . f) a >>= g >>= ok' req accept cache
    m16 req >>= \case
        Right () -> ok'' RDelete deleteResource
        Left () -> n16 req >>= \case
            Right () -> ok'' RPost postResource
            Left () -> o16 req >>= \case
                Right () -> ok'' RPut putResource
                Left () -> ok'' ROther return

resourceNotFound :: Monad m => Request -> MediaType -> (forall a. ResourceNotFoundMethod a -> m a) -> FlowStateT m ()
resourceNotFound req accept f = do
  _ <- g07 Nothing
  h07 req
  i07 req >>= \case
    Right () ->
      lift (f MPut) >>= putNewResource req accept
    Left () ->
      -- Peeking ahead to l7/m5 so we can use better types
      case (==) HTTP.methodPost . requestMethod $ req of
        True ->
          lift (f MPost) >>= postNewResource req accept
        False ->
          lift (f MOther) >>= otherNewResource req

deleteResource :: Monad m => RDeleteAction -> FlowStateT m ResponseEntity
deleteResource = \case
  RDeleteAccepted -> do
    m20 False
    flowStateHalt HTTP.status500
  RDeleteOk re -> do
    m20 True
    return re

putResource :: Monad m => RPutAction -> FlowStateT m ResponseEntity
putResource = \case
  RPutConflict -> do
    o14 True
    flowStateHalt HTTP.status500
  RPutCreated l -> do
    o14 False
    p11 $ Just l
    flowStateHalt HTTP.status500
  RPutOk re -> do
    o14 False
    p11 Nothing
    return re

postResource :: Monad m => RPostAction -> FlowStateT m ResponseEntity
postResource = \case
  RPostSeeOther l -> do
    n11 $ Just l
    flowStateHalt HTTP.status500
  RPostCreated l -> do
    n11 Nothing
    p11 $ Just l
    flowStateHalt HTTP.status500
  RPostOk re -> do
    n11 Nothing
    p11 Nothing
    return re

postNewResource :: Monad m => Request -> MediaType -> MPostAction -> FlowStateT m ()
postNewResource req accept = \case
  MPostMoved moved -> do
    _ <- k07 PreviouslyExisted
    movedResource moved
  MPostGone -> do
    _ <- k07 PreviouslyExisted
    l07 req
    k05 Nothing
    l05 Nothing
    m05 req
    n05 False
  MPostNotFound -> do
    _ <- k07 NeverSeenBefore
    l07 req
    m07 False
  MPostSeeOther loc -> do
    _ <- k07 NeverSeenBefore
    l07 req
    m07 True
    n11 $ Just loc
  MPostRedirect loc -> do
    _ <- k07 NeverSeenBefore
    l07 req
    m07 True
    n11 Nothing
    p11 $ Just loc
  MPostOk cache b -> do
    _ <- k07 NeverSeenBefore
    l07 req
    m07 True
    n11 Nothing
    p11 Nothing
    ok' req accept cache b

otherNewResource :: Monad m => Request -> MOtherAction -> FlowStateT m ()
otherNewResource req = \case
  MOtherMoved moved -> do
    _ <- k07 PreviouslyExisted
    movedResource moved
  MOtherGone -> do
    _ <- k07 PreviouslyExisted
    k05 Nothing
    l05 Nothing
    m05 req
  MOtherNotFound -> do
    _ <- k07 NeverSeenBefore
    l07 req

movedResource :: Monad m => Moved -> FlowStateT m ()
movedResource = \case
  MovedPermanently b ->
    k05 $ Just b
  MovedTemporarily b -> do
    k05 Nothing
    l05 $ Just b

putNewResource :: Monad m => Request -> MediaType -> MPutAction -> FlowStateT m ()
putNewResource req accept = \case
  MPutMoved l ->
    i04 $ Just l
  MPutConflict -> do
    i04 Nothing
    p03 True
  MPutCreated l -> do
    i04 Nothing
    p03 False
    p11 $ Just l
  MPutOk cache re -> do
    i04 Nothing
    p03 False
    p11 Nothing
    ok' req accept cache re

ok' :: Monad m => Request -> MediaType -> CacheData -> ResponseEntity -> FlowStateT m ()
ok' req mt cache (ResponseEntity mr b) = do
  o20 b
  setResponseBody b $
    o18 (req, mr == MultipleRepresentations, cache, mt)

newResource :: Monad m => Request -> ResourceDesc a b -> m (Either ResourceDenied c) -> FlowStateT m (a, (MediaType, b), c)
newResource req (ResourceDesc available methods contentTypes accept lang) rd = do
  b13 available
  b12 req
  b10 (req, methods)
  (contentType, c) <- lift rd >>= \case
    Left rd' -> do
      b11 rd'
      b09 rd'
      b08 rd'
      b07 rd'
      b06 rd'
      _ <- b05 (req, [])
      b04 rd'
      -- This is because we're not pattern matching so that the trace is correct
      flowStateHalt HTTP.status500
    Right c -> do
       a <- liftM (fromMaybe (snd $ NE.head contentTypes)) . b05 . (,) req $ NE.toList contentTypes
       return (a, c)
  b03 (req, methods)
  b <- liftM (either id id . first (\() -> NE.head accept)) $
    c03 req >>= mapM (\a -> c04 (a, NE.toList accept))
  d04 req >>= mapM_ (d05 . lang)
  e05 req >>= mapM_ (e06)
  f06 req >>= mapM_ (f07)
  return (contentType, b, c)

preconditions :: Monad m => Request -> UTCTime -> CacheData -> FlowStateT m ()
preconditions req now (CacheData modified' etag) = do
  ifMatch' req etag
  ifUnmodifiedSince' req modified'
  ifNoneMatch' req etag
  ifModifiedSince' req now modified'

ifMatch' :: Monad m => Request -> Maybe ETag -> FlowStateT m ()
ifMatch' req etag =
  g08 req >>= mapM_ (g09 >=> either (g11 . (,) etag) return)

ifUnmodifiedSince' :: Monad m => Request -> Maybe UTCTime -> FlowStateT m ()
ifUnmodifiedSince' req lastModified =
  h10 req
    >>= mapM_ (h11 >=> mapM_ (h12 . (,) lastModified))

ifNoneMatch' :: Monad m => Request -> Maybe ETag -> FlowStateT m ()
ifNoneMatch' req etag =
  i12 req
    >>= mapM_ (i13 >=>
      either
        (k13 . (,) etag >=> either return (\_ -> j18 req))
        (\() -> j18 req)
      )

ifModifiedSince' :: Monad m => Request -> UTCTime -> Maybe UTCTime -> FlowStateT m ()
ifModifiedSince' req now lastModified =
  l13 req
    >>= mapM_ (l14 >=> mapM (
          (l15 . (,) now) >=> either (l17 . (,) lastModified) return
        )
      )

flowStateHalt :: Monad m => Status -> FlowStateT m a
flowStateHalt =
  FlowStateT . left . (,) Empty

newFlowStateT  :: Monad m => a -> Decision i x y -> FlowStateT m a
newFlowStateT a d =
  FlowStateT . EitherT . liftM Right $ writer (a, ([decisionName d], mempty))

leftFlowStateT  :: Monad m => CommittedResponse -> Decision i x y -> FlowStateT m a
leftFlowStateT (CommittedResponse s p) d =
  FlowStateT . EitherT $ tell ([decisionName d], p) >> return (Left $ (Empty, s))

decisionEval :: Monad m => Decision i a b -> i -> FlowStateT m (Either a b)
decisionEval d i =
  newFlowStateT (decisionRun d i) d

decisionEvalT :: Monad m => DecisionT i a -> i -> FlowStateT m a
decisionEvalT d i =
  case decisionRun d i of
    Left c ->
      leftFlowStateT c d
    Right a ->
      newFlowStateT a d

decisionEvalF :: Monad m => DecisionF i a -> i -> FlowStateT m a
decisionEvalF d i =
  case decisionRun d i of
    Left a ->
      newFlowStateT a d
    Right c ->
      leftFlowStateT c d

decisionEvalH :: Monad m => DecisionH i -> i -> FlowStateT m ()
decisionEvalH d i =
  leftFlowStateT (either id id $ decisionRun d i) d

-- FIX This is a small, how can we avoid this?
setResponseBody :: Monad m => ResponseBody -> FlowStateT m a -> FlowStateT m a
setResponseBody b (FlowStateT e) =
  FlowStateT . mapEitherT (liftM (first (first (const b)))) $ e



---
--- FIX Can we not define these twice somehow?
---

b03 :: Monad m => (Request, [HTTP.Method]) -> FlowStateT m ()
b03 = decisionEvalF D.b03
b04 :: Monad m => ResourceDenied -> FlowStateT m ()
b04 = decisionEvalF D.b04
b05 :: Monad m => (Request, [(MediaType, a)]) -> FlowStateT m (Maybe a)
b05 = decisionEvalF D.b05
b06 :: Monad m => ResourceDenied -> FlowStateT m ()
b06 = decisionEvalF D.b06
b07 :: Monad m => ResourceDenied -> FlowStateT m ()
b07 = decisionEvalF D.b07
b08 :: Monad m => ResourceDenied -> FlowStateT m ()
b08 = decisionEvalT D.b08
b09 :: Monad m => ResourceDenied -> FlowStateT m ()
b09 = decisionEvalF D.b09
b10 :: Monad m => (Request, [HTTP.Method]) -> FlowStateT m ()
b10 = decisionEvalT D.b10
b11 :: Monad m => ResourceDenied -> FlowStateT m ()
b11 = decisionEvalF D.b11
b12 :: Monad m => Request -> FlowStateT m ()
b12 = decisionEvalT D.b12
b13 :: Monad m => Available -> FlowStateT m ()
b13 = decisionEvalT D.b13
c03 :: Monad m => Request -> FlowStateT m (Either () AcceptHeader)
c03 = decisionEval D.c03
c04 :: Monad m => (AcceptHeader, [(MediaType, a)]) -> FlowStateT m (MediaType, a)
c04 = decisionEvalT D.c04
d04 :: Monad m => Request -> FlowStateT m (Either () AcceptLanguage)
d04 = decisionEval D.d04
d05 :: Monad m => Bool -> FlowStateT m ()
d05 = decisionEvalT D.d05
e05 :: Monad m => Request -> FlowStateT m (Either () AcceptCharset)
e05 = decisionEval D.e05
e06 :: Monad m => AcceptCharset -> FlowStateT m ()
e06 = decisionEvalT D.e06
f06 :: Monad m => Request -> FlowStateT m (Either () AcceptEncoding)
f06 = decisionEval D.f06
f07 :: Monad m => AcceptEncoding -> FlowStateT m ()
f07 = decisionEvalT D.f07
g07 :: Monad m => Maybe a -> FlowStateT m (Either () a)
g07 = decisionEval D.g07
g08 :: Monad m => Request -> FlowStateT m (Either () IfMatch)
g08 = decisionEval D.g08
g09 :: Monad m => IfMatch -> FlowStateT m (Either IfMatch ())
g09 = decisionEval D.g09
g11 :: Monad m => (Maybe ETag, IfMatch) -> FlowStateT m ()
g11 = decisionEvalT D.g11
h07 :: Monad m => Request -> FlowStateT m ()
h07 = decisionEvalF D.h07
h10 :: Monad m => Request -> FlowStateT m (Either () IfUnmodifiedSinceHeader)
h10 = decisionEval D.h10
h11 :: Monad m => IfUnmodifiedSinceHeader -> FlowStateT m (Either () IfUnmodifiedSince)
h11 = decisionEval D.h11
h12 :: Monad m => (Maybe UTCTime, IfUnmodifiedSince) -> FlowStateT m ()
h12 = decisionEvalF D.h12
i04 :: Monad m => (Maybe Location) -> FlowStateT m ()
i04 = decisionEvalF D.i04
i07 :: Monad m => Request -> FlowStateT m (Either () ())
i07 = decisionEval D.i07
i12 :: Monad m => Request -> FlowStateT m (Either () IfNoneMatch)
i12 = decisionEval D.i12
i13 :: Monad m => IfNoneMatch -> FlowStateT m (Either IfNoneMatch ())
i13 = decisionEval D.i13
j18 :: Monad m => Request -> FlowStateT m ()
j18 = decisionEvalH D.j18
k05 :: Monad m => (Maybe Location) -> FlowStateT m ()
k05 = decisionEvalF D.k05
k07 :: Monad m => PreviouslyExisted -> FlowStateT m (Either () ())
k07 = decisionEval D.k07
k13 :: Monad m => (Maybe ETag, IfNoneMatch) -> FlowStateT m (Either () (Maybe ETag))
k13 = decisionEval D.k13
l05 :: Monad m => (Maybe Location) -> FlowStateT m ()
l05 = decisionEvalF D.l05
l07 :: Monad m => Request -> FlowStateT m ()
l07 = decisionEvalT D.l07
l13 :: Monad m => Request -> FlowStateT m (Either () IfModifiedSinceHeader)
l13 = decisionEval D.l13
l14 :: Monad m => IfModifiedSinceHeader -> FlowStateT m (Either () IfModifiedSince)
l14 = decisionEval D.l14
l15 :: Monad m => (UTCTime, IfModifiedSince) -> FlowStateT m (Either IfModifiedSince ())
l15 = decisionEval D.l15
l17 :: Monad m => (Maybe UTCTime, IfModifiedSince) -> FlowStateT m ()
l17 = decisionEvalT D.l17
m05 :: Monad m => Request -> FlowStateT m ()
m05 = decisionEvalT D.m05
m07 :: Monad m => Bool -> FlowStateT m ()
m07 = decisionEvalT D.m07
m16 :: Monad m => Request -> FlowStateT m (Either () ())
m16 = decisionEval D.m16
m20 :: Monad m => Bool -> FlowStateT m ()
m20 = decisionEvalT D.m20
n05 :: Monad m => Bool -> FlowStateT m ()
n05 = decisionEvalT D.n05
n11 :: Monad m => (Maybe Location) -> FlowStateT m ()
n11 = decisionEvalF D.n11
n16 :: Monad m => Request -> FlowStateT m (Either () ())
n16 = decisionEval D.n16
o14 :: Monad m => Bool -> FlowStateT m ()
o14 = decisionEvalF D.o14
o16 :: Monad m => Request -> FlowStateT m (Either () ())
o16 = decisionEval D.o16
o18 :: Monad m => (Request, Bool, CacheData, MediaType) -> FlowStateT m ()
o18 = decisionEvalH D.o18
o20 :: Monad m => ResponseBody -> FlowStateT m ()
o20 = decisionEvalT D.o20
p03 :: Monad m => Bool -> FlowStateT m ()
p03 = decisionEvalF D.p03
p11 :: Monad m => (Maybe Location) -> FlowStateT m ()
p11 = decisionEvalF D.p11
