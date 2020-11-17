module Core.Interactor.CreateComment
  ( run
  , Handle(..)
  , GatewayFailure(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.Comment
import Core.EntityId
import Core.Exception
import Core.News
import Core.User
import qualified Data.Text as T
import Data.Time

data Handle m =
  Handle
    { hCreateComment :: T.Text -> Maybe UserId -> NewsId -> UTCTime -> m (Either GatewayFailure Comment)
    , hGetCurrentTime :: m UTCTime
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> T.Text
  -> NewsId
  -> m Comment
run Handle {..} authUser text newsId' = do
  createdAt <- hGetCurrentTime
  hCreateComment text (authenticatedUserId authUser) newsId' createdAt >>=
    either throwFailure pure
  where
    throwFailure (GUnknownEntityId entityId) =
      throwM $ DependentEntitiesNotFoundException [entityId]

newtype GatewayFailure =
  GUnknownEntityId EntityId
