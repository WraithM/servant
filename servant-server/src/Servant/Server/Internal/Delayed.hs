{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Servant.Server.Internal.Delayed where

import Control.Monad (liftM2)

import Servant.Server.Internal.RoutingApplication hiding (Delayed)

-- * Sanity types
-- These types are used so field access functions have a chance of being
-- properly maintained. Due to https://ghc.haskell.org/trac/ghc/ticket/2595,
-- records accessors can't actually be used.

data MethodCheck = MethodCheck
data AuthorizedCheck = AuthorizedCheck
data PayloadSizeCheck = PayloadSizeCheck
data ContentTypeCheck = ContentTypeCheck
data ForbiddenCheck = ForbiddenCheck
data AcceptCheck = AcceptCheck
data AcceptLanguageCheck = AcceptLanguageCheck

data Delayed result = forall captures body . Delayed
    -- | Deserialize capture. Failure: 404
    (RouteResultM captures)
    -- | Check method. Failure: 405
    (RouteResultM MethodCheck)
    -- | Check if user is authorized. Failure: 401
    (RouteResultM AuthorizedCheck)
    -- | Check whether payload is too large. Failure: 413
    (RouteResultM PayloadSizeCheck)
    -- | Check whether content-type can be handled. Failure: 415
    (RouteResultM ContentTypeCheck)
    -- | Check whether user is forbidden. Failure: 403
    (RouteResultM ForbiddenCheck)
    -- | Try deserializing body. Failure: 415
    (RouteResultM body)
    -- | Check whether the @Accept@ header can be handler. Failure: 406.
    (RouteResultM AcceptCheck)
    -- | Check whether the @Accept-Language@ header can be handler. Failure: 406.
    (RouteResultM AcceptLanguageCheck)
    -- | The actual handler
    (captures -> body -> RouteResultM result)

deriving instance Functor Delayed

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

