{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Webship.Decision.Core where

import qualified Data.ByteString as BS
import           Data.List (lookup)
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)

import           Network.HTTP.Media
import           Network.HTTP.Types (Header, Status)
import qualified Network.HTTP.Types as HTTP
import           Network.Wai (Request (..))

import           P

import           Webship.Decision.Data
import           Webship.Http
import           Webship.Wai


data DecisionName =
  DecisionName {
      renderDecisionName :: Text
    } deriving (Eq, Show)

-- FIX Use a QQ instead
instance IsString DecisionName where
  fromString =
    DecisionName . T.pack

data Decision i a b =
  Decision {
      decisionName :: DecisionName
    , decisionFalse :: DecisionEdge
    , decisionTrue :: DecisionEdge
    , decisionRun :: i -> Either a b
    }

type DecisionF i a = Decision i a CommittedResponse
type DecisionT i a = Decision i CommittedResponse a
type DecisionH i = Decision i CommittedResponse CommittedResponse

data DecisionEdge =
    DecisionEdgeStatus Status
  | DecisionEdgeNode DecisionName

data PartialResponse =
  PartialResponse [Header]

data CommittedResponse =
  CommittedResponse Status PartialResponse

instance Monoid PartialResponse where
  mempty =
    PartialResponse []
  mappend (PartialResponse ha) (PartialResponse hb) =
    PartialResponse $ ha <> hb

haltF :: Monoid a => (i -> Bool) -> l -> i -> Either l a
haltF b l i =
  if b i
    then pure mempty
    else Left l

haltT :: Monoid a => (i -> Bool) -> r -> i -> Either a r
haltT b r i =
  if b i
    then Right r
    else Left mempty

decision :: DecisionName -> Decision x1 y1 z1 -> Decision x2 y2 z2 -> (i -> Either a b) -> Decision i a b
decision n f t =
  Decision n (DecisionEdgeNode $ decisionName f) (DecisionEdgeNode $ decisionName t)

decisionF :: DecisionName -> Decision x y z -> Status -> (i -> Either a PartialResponse) -> DecisionF i a
decisionF n d s =
  Decision n (DecisionEdgeStatus s) (DecisionEdgeNode $ decisionName d) . fmap (second (CommittedResponse s))

decisionT :: DecisionName -> Status -> Decision x y z -> (i -> Either PartialResponse a) -> DecisionT i a
decisionT n s d = do
  Decision n (DecisionEdgeNode $ decisionName d) (DecisionEdgeStatus s) . fmap (first (CommittedResponse s))

decisionH :: DecisionName -> Status -> Status -> (i -> Either PartialResponse PartialResponse) -> DecisionH i
decisionH n f t =
  Decision n (DecisionEdgeStatus f) (DecisionEdgeStatus t) . fmap (bimap (CommittedResponse f) (CommittedResponse t))

boolE :: a -> b -> Bool -> Either a b
boolE a b =
  bool (Left a) (Right b)

------------------------------------------------------------------------------
-- B column
------------------------------------------------------------------------------

b13 :: DecisionT Available ()
b13 =
  decisionT "b13" HTTP.status503 b12 $
    haltT (Available ==) ()

b12 :: DecisionT Request ()
b12 =
  decisionT "b12" HTTP.status501 b11 $
    haltT (flip elem knownMethods . requestMethod) ()
  where
    knownMethods = [
        HTTP.methodGet
      , HTTP.methodPost
      , HTTP.methodHead
      , HTTP.methodPut
      , HTTP.methodDelete
      , HTTP.methodTrace
      , HTTP.methodConnect
      , HTTP.methodOptions
      , HTTP.methodPatch
      ]

b11 :: DecisionF ResourceDenied ()
b11 =
  decisionF "b11" b10 HTTP.status414 $
    haltF (UriTooLong ==) ()

b10 :: DecisionT (Request, [HTTP.Method]) ()
b10 =
  decisionT "b10" HTTP.status405 b09 $ \(req, allowed) ->
    if elem (requestMethod req) allowed
      then return ()
      else Left $ PartialResponse [("Allow", BS.intercalate "," allowed)]

b09 :: DecisionF ResourceDenied ()
b09 =
  decisionF "b09" b08 HTTP.status400 $
    haltF (MalformedRequest ==) ()

b08 :: DecisionT ResourceDenied ()
b08 =
  decisionT "b08" HTTP.status401 b07 $
    haltT (Unauthorized /=) ()

b07 :: DecisionF ResourceDenied ()
b07 =
  decisionF "b07" b06 HTTP.status403 $
    haltF (Forbidden ==) ()

b06 :: DecisionF ResourceDenied ()
b06 =
  decisionF "b06" b05 HTTP.status501 $
    haltF (UnsupportedContentType ==) ()

b05 :: DecisionF (Request, [(MediaType, a)]) (Maybe a)
b05 =
  decisionF "b05" b04 HTTP.status415 $ \(req, known) ->
    let headers = requestHeaders req
    in case lookup HTTP.hContentType headers of
      Nothing -> Left Nothing
      Just t -> case mapAcceptMedia known t of
        Just a -> Left $ Just a
        Nothing -> pure mempty

b04 :: DecisionF ResourceDenied ()
b04 =
  decisionF "b04" b03 HTTP.status413 $
    haltF (EntityTooLarge ==) ()

b03 :: DecisionF (Request, [HTTP.Method]) ()
b03 =
  decisionF "b03" c03 HTTP.status204 $ \(req, allowed) ->
    if requestMethod req == HTTP.methodOptions
      then return $ PartialResponse [("Allow", BS.intercalate "," allowed)]
      else Left ()

------------------------------------------------------------------------------
-- C column
------------------------------------------------------------------------------

c04 :: DecisionT (AcceptHeader, [(MediaType, a)]) (MediaType, a)
c04 =
  decisionT "c04" HTTP.status406 d04 $ \(AcceptHeader acceptStr, provided) ->
    let
      result = mapAcceptMedia provided' acceptStr
      -- this is so that in addition to getting back the resource
      -- that we match, we also return the content-type provided
      -- by that resource.
      provided' = fmap dupContentType provided
      dupContentType (a, b) = (a, (a, b))
    in case result of
      Nothing -> Left mempty
      Just res -> return res

c03 :: Decision Request () AcceptHeader
c03 =
  decision "c03" d04 c03 $
    maybeToRight () . fmap AcceptHeader . lookup HTTP.hAccept . requestHeaders

------------------------------------------------------------------------------
-- D column
------------------------------------------------------------------------------

d05 :: DecisionT Bool ()
d05 =
  decisionT "d05" HTTP.status406 e05 $
    haltT id ()

d04 :: Decision Request () AcceptLanguage
d04 =
  decision "d04" e05 d05 $
    maybeToRight () . fmap AcceptLanguage . lookup HTTP.hAcceptLanguage . requestHeaders

------------------------------------------------------------------------------
-- E column
------------------------------------------------------------------------------

e06 :: DecisionT AcceptCharset ()
e06 =
  decisionT "e06" HTTP.status406 f06 $ \_ ->
    -- TODO: charset negotiation
    return ()

e05 :: Decision Request () AcceptCharset
e05 =
  decision "e05" f06 e06 $
    maybeToRight () . fmap AcceptCharset . lookup hAcceptCharset . requestHeaders


------------------------------------------------------------------------------
-- F column
------------------------------------------------------------------------------

f07 :: DecisionT AcceptEncoding ()
f07 =
  decisionT "f07" HTTP.status406 g07 $ \_ ->
    -- TODO: encoding negotiation
    return ()

f06 :: Decision Request () AcceptEncoding
f06 =
  decision "f06" g07 f07 $
    maybeToRight () . fmap AcceptEncoding . lookup hAcceptEncoding . requestHeaders

------------------------------------------------------------------------------
-- G column
------------------------------------------------------------------------------

g11 :: DecisionT (Maybe ETag, IfMatch) ()
g11  =
  decisionT "g11" HTTP.status412 h10 $
    haltT (\(etag, IfMatch ifMatch) -> any (flip elem etag) . parseEtagList $ ifMatch) ()

g09 :: Decision IfMatch IfMatch ()
g09 =
  decision "g09" g11 h10 $ \ifMatch ->
    case ifMatch of
      -- TODO: should we be stripping whitespace here?
      (IfMatch "*") ->
        Right ()
      _ ->
        Left ifMatch

g08 :: Decision Request () IfMatch
g08 =
  decision "g08" h10 g09 $
    maybeToRight () . fmap IfMatch . lookup hIfMatch . requestHeaders

g07 :: Decision (Maybe a) () a
g07 =
  decision "g07" h07 g08 $
    maybeToRight ()

------------------------------------------------------------------------------
-- H column
------------------------------------------------------------------------------

h12 :: DecisionF (Maybe UTCTime, IfUnmodifiedSince) ()
h12 =
  decisionF "h12" i12 HTTP.status412 $
    haltF (\(modified, IfUnmodifiedSince headerDate) -> any (> headerDate) modified) ()

h11 :: Decision IfUnmodifiedSinceHeader () IfUnmodifiedSince
h11 =
  decision "h11" i12 h12 $ \(IfUnmodifiedSinceHeader header) ->
    maybeToRight () . fmap IfUnmodifiedSince $ parseRfc1123Date header

h10 :: Decision Request () IfUnmodifiedSinceHeader
h10 =
  decision "h10" i12 h11 $
    maybeToRight () . fmap IfUnmodifiedSinceHeader . lookup hIfUnmodifiedSince . requestHeaders

h07 :: DecisionF Request ()
h07 =
  decisionF "h07" i07 HTTP.status412 $
    -- TODO: should we be stripping whitespace here?
    haltF (elem "*" . lookup hIfMatch . requestHeaders) ()

------------------------------------------------------------------------------
-- I column
------------------------------------------------------------------------------

i13 :: Decision IfNoneMatch IfNoneMatch ()
i13 =
  decision "i13" k13 j18 $ \ifNoneMatch ->
    -- TODO: should we be stripping whitespace here?
    if ifNoneMatch == IfNoneMatch "*"
      then Right ()
      else Left ifNoneMatch

i12 :: Decision Request () IfNoneMatch
i12 =
  decision "i12" l13 i13 $
    maybeToRight () . fmap IfNoneMatch . lookup hIfNoneMatch . requestHeaders

i07 :: Decision Request () ()
i07 =
  decision "i07" k07 i04 $
    boolE () () . (HTTP.methodPut ==) . requestMethod

i04 :: DecisionF (Maybe Location) ()
i04 =
  decisionF "i04" p03 HTTP.status301 $ \case
    Just (Location loc) ->
      Right $ PartialResponse [("Location", loc)]
    Nothing ->
      Left ()

------------------------------------------------------------------------------
-- J column
------------------------------------------------------------------------------

j18 :: DecisionH Request
j18 =
  decisionH "j18" HTTP.status412 HTTP.status304 $
    boolE mempty mempty . flip elem [HTTP.methodGet, HTTP.methodHead] . requestMethod

------------------------------------------------------------------------------
-- K column
------------------------------------------------------------------------------

k13 :: Decision (Maybe ETag, IfNoneMatch) () (Maybe ETag)
k13 =
  decision "k13" l13 j18 $ \(etag, IfNoneMatch ifNoneMatch) ->
    boolE () etag . any (flip elem etag) $ parseEtagList ifNoneMatch

k07 :: Decision PreviouslyExisted () ()
k07 =
  decision "k07" l07 k05 $
    boolE () () . (PreviouslyExisted ==)

k05 :: DecisionF (Maybe Location) ()
k05 =
  decisionF "k05" l05 HTTP.status301 $
    maybeToRight () . fmap (\(Location loc) -> PartialResponse [("Location", loc)])

------------------------------------------------------------------------------
-- L column
------------------------------------------------------------------------------

l17 :: DecisionT (Maybe UTCTime, IfModifiedSince) ()
l17 =
  decisionT "l17" HTTP.status304 m16 $
    haltT (\(modified, IfModifiedSince ifModifiedSince) -> any (> ifModifiedSince) modified) ()

l15 :: Decision (UTCTime, IfModifiedSince) IfModifiedSince ()
l15 =
  decision "l15" l17 m16 $ \(now, IfModifiedSince ifModifiedSince) ->
    boolE (IfModifiedSince ifModifiedSince) () $ ifModifiedSince > now

l14 :: Decision IfModifiedSinceHeader () IfModifiedSince
l14 =
  decision "l14" m16 l15 $ \(IfModifiedSinceHeader dateHeader) ->
    maybeToRight () . fmap IfModifiedSince . parseRfc1123Date $ dateHeader

l13 :: Decision Request () IfModifiedSinceHeader
l13 =
  decision "l13" m16 l14 $
    maybeToRight () . fmap IfModifiedSinceHeader . lookup HTTP.hIfModifiedSince . requestHeaders

l07 :: DecisionT Request ()
l07 =
  decisionT "l07" HTTP.status404 m07 $
    haltT ((HTTP.methodPost ==) . requestMethod) ()

l05 :: DecisionF (Maybe Location) ()
l05 =
  decisionF "l05" m05 HTTP.status307 $
    maybeToRight () . fmap (\(Location loc) -> PartialResponse [("Location", loc)])

------------------------------------------------------------------------------
-- M column
------------------------------------------------------------------------------

m20 :: DecisionT Bool ()
m20 =
  decisionT "m20" HTTP.status202 o20 $
    haltT id ()

m16 :: Decision Request () ()
m16 =
  decision "m16" n16 m20 $
    boolE () () . (HTTP.methodDelete ==) . requestMethod

m07 :: DecisionT Bool ()
m07 =
  decisionT "m07" HTTP.status404 n11 $
    haltT id ()

m05 :: DecisionT Request ()
m05 =
  decisionT "m05" HTTP.status410 n05 $
    haltT ((HTTP.methodPost ==) . requestMethod) ()

------------------------------------------------------------------------------
-- N column
------------------------------------------------------------------------------

n16 :: Decision Request () ()
n16 =
  decision "n16" o16 n11 $
    boolE () () . (HTTP.methodPost ==) . requestMethod

n11 :: DecisionF (Maybe Location) ()
n11 =
  decisionF "n11" p11 HTTP.status303 $
    maybeToRight () . fmap (\(Location loc) -> PartialResponse [(HTTP.hLocation, loc)])

n05 :: DecisionT Bool ()
n05 =
  decisionT "n05" HTTP.status410 n11 $
    haltT id ()

------------------------------------------------------------------------------
-- O column
------------------------------------------------------------------------------

o20 :: DecisionT ResponseBody ()
o20 =
  decisionT "o20" HTTP.status204 o18 $ \body ->
    -- ResponseBody is a little tough to make an instance of 'Eq',
    -- so we just use a pattern match
    case body of
      Empty -> Left mempty
      _ -> Right ()

o18 :: DecisionH (Request, Bool, CacheData, MediaType)
o18 =
  decisionH "o18" HTTP.status200 HTTP.status300 $ \(req, multiple, CacheData modified' etag', cType) ->
    if multiple
      then Right mempty
      else
        -- TODO: set etag, expiration, etc. headers
        Left . PartialResponse . catMaybes $ [
            valueOrEmpty
              (elem (requestMethod req) [HTTP.methodGet, HTTP.methodHead])
              ("Content-Type", renderHeader cType)
          , flip fmap modified' $ (,) "Last-Modified" . utcTimeToRfc1123
          , flip fmap etag' $ (,) "ETag" . etagToByteString
          ]

o16 :: Decision Request () ()
o16 =
  decision "o16" o18 o14 $
    boolE () () . (HTTP.methodPut ==) . requestMethod

o14 :: DecisionF Bool ()
o14 =
  decisionF "o14" p11 HTTP.status409 $
    haltF id ()

------------------------------------------------------------------------------
-- P column
------------------------------------------------------------------------------

p11 :: DecisionF (Maybe Location) ()
p11 =
  decisionF "p11" o20 HTTP.status201 $
    maybeToRight () . fmap (\(Location loc) -> PartialResponse [(HTTP.hLocation, loc)])

p03 :: DecisionF Bool ()
p03 =
  decisionF "p03" p11 HTTP.status409 $
    haltF id ()
