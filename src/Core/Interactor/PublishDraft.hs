module Core.Interactor.PublishDraft
  ( run
  , Handle(..)
  , GatewayFailure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.EntityId
import Core.Exception
import Core.News
import Data.Time

data Handle m =
  Handle
    { hAuthenticationHandle :: AuthenticationHandle m
    , hAuthorizationHandle :: AuthorizationHandle
    , hGetAuthorOfNewsVersion :: NewsVersionId -> m (Either GatewayFailure AuthorId)
    , hGetCurrentDay :: m Day
    , hCreateNews :: NewsVersionId -> Day -> m News
    }

run :: MonadThrow m => Handle m -> Maybe Credentials -> NewsVersionId -> m News
run Handle {..} credentials vId = do
  actor <- authenticate hAuthenticationHandle credentials
  documentAuthorId <- hGetAuthorOfNewsVersion vId >>= fromGatewayResult vId
  requireAuthorshipPermission
    hAuthorizationHandle
    documentAuthorId
    actor
    "publish a draft"
  day <- hGetCurrentDay
  hCreateNews vId day

data GatewayFailure =
  UnknownNewsVersionId

fromGatewayResult ::
     MonadThrow m => NewsVersionId -> Either GatewayFailure a -> m a
fromGatewayResult vId = either (throwM . exceptionFromGatewayFailure vId) pure

exceptionFromGatewayFailure :: NewsVersionId -> GatewayFailure -> CoreException
exceptionFromGatewayFailure vId UnknownNewsVersionId =
  RequestedEntityNotFoundException $ toEntityId vId
