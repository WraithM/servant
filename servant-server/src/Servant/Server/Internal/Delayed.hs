{-# LANGUAGE ExistentialQuantification #-}
module Servant.Server.Internal.Delayed where

import Control.Monad (liftM2)
import Data.Functor.Contravariant
import Data.ByteString

import Servant.Server.Internal.RoutingApplication hiding (Delayed)

-- * Sanity types
-- These types are used so field access functions have a chance of being
-- properly maintained. Due to https://ghc.haskell.org/trac/ghc/ticket/2595,
-- records accessors can't actually be used.

-- These get fixed later
data DeserializationError
data Language
data Charset
data Encoding
data ServantErr

data Delayed result = forall captures body . Delayed
    (RequestDelayed captures body)
    (AcceptDelayed result)
    PreconditionDelayed
    (captures -> body -> RouteResultM result) -- ^ the actual handler

data RequestDelayed captures body = RequestDelayed {
    -- | Deserialize capture. Failure: 404
    _captureCheck :: RouteResultM captures
    -- | Check method. Failure: 405
  , _methodCheck  :: RouteResultM ()
    -- | Check if user is authorized. Failure: 401
  , _authorizedCheck :: RouteResultM ()
    -- | Check whether payload is too large. Failure: 413
  , _payloadSizeCheck :: RouteResultM ()
    -- | Check whether content-type can be handled. Failure: 415
  , _contentTypeCheck :: RouteResultM (ByteString -> Either DeserializationError body)
    -- | Check whether user is forbidden. Failure: 403
  , _forbiddenCheck :: RouteResultM ()
    -- | Try deserializing body. Failure: 400
  , _bodyCheck :: RouteResultM ((ByteString -> Either DeserializationError body) -> body)
    -- | Handle deserialization error.
  , _deserializationHandler :: DeserializationError -> ServantErr
    }

captureCheck :: _
captureCheck = lens _captureCheck _ -- (\s b -> s { _captureCheck s = b })

instance Functor (RequestDelayed captures) where
    fmap f rd = rd { _contentTypeCheck = (fmap.fmap) f <$> _contentTypeCheck rd
                   , _bodyCheck = go f <$> _bodyCheck rd
                   }
        where
          go :: (a -> b) -> ((c -> Either d a) -> a) -> (c -> Either d b) -> b
          go f g h = undefined --- whatever, I'm drunk, but this works


data AcceptDelayed result = AcceptDelayed {
    -- | Check whether the @Accept@ header can be handler. Failure: 406.
    _acceptCheck :: RouteResultM (result -> ByteString)
    -- | Check whether the @Accept-Language@ header can be handler. Failure: 406.
  , _acceptLanguageCheck :: RouteResultM Language
    -- | Check whether the @Accept-Charset@ header can be handler. Failure: 406.
  , _acceptCharsetCheck :: RouteResultM Charset
    -- | Check whether the @Accept-Encoding@ header can be handler. Failure: 406.
  , _acceptEncodingCheck :: RouteResultM Encoding
  }

instance Contravariant AcceptDelayed where
    contramap f ad = ad { _acceptCheck = (. f) <$> _acceptCheck ad }

data PreconditionDelayed = PreconditionDelayed {
    _ifMatchCheck :: RouteResultM ()
  , _ifUnmodifiedSinceCheck :: RouteResultM ()
  , _ifNoneMatchCheck :: RouteResultM ()
  , _ifModifiedSinceCheck :: RouteResultM ()
  }



{-
addCapture :: Delayed (a -> b) -> RouteResultM a -> Delayed b
addCapture (Delayed captures a b c d e f g h handler) new =
    let captures' = liftM2 (,) captures new
        handler' (old, v) y = ($ v) <$> handler old y
    in Delayed captures' a b c d e f g h handler'

addMethod :: Delayed a -> RouteResultM a -> Delayed a
addMethod (Delayed a method b c d e f g h handler) new =
    let method' = liftM2 const method new
    in Delayed a method' b c d e f g h handler

addAuthorized :: Delayed a -> RouteResultM a -> Delayed a
addAuthorized (Delayed a b authorized c d e f g h handler) new =
    let authorized' = liftM2 const authorized new
    in Delayed a b authorized' c d e f g h handler

addPayload :: Delayed a -> RouteResultM a -> Delayed a
addPayload (Delayed a b c payload d e f g h handler) new =
    let payload' = liftM2 const payload new
    in Delayed a b c payload' d e f g h handler
-}
