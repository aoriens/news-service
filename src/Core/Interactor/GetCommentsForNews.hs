module Core.Interactor.GetCommentsForNews
  ( run
  , Handle(..)
  , GatewayFailure(..)
  ) where

import Control.Monad.Catch
import Core.Comment
import Core.EntityId
import Core.Exception
import Core.News
import Core.Pagination

data Handle m =
  Handle
    { hGetCommentsForNews :: NewsId -> PageSpec -> m (Either GatewayFailure [Comment])
    , hPageSpecParserHandle :: PageSpecParserHandle
    }

run :: MonadThrow m => Handle m -> NewsId -> PageSpecQuery -> m [Comment]
run Handle {..} newsId pageQuery = do
  pageSpec <- parsePageSpecM hPageSpecParserHandle pageQuery
  hGetCommentsForNews newsId pageSpec >>= either throwFailure pure
  where
    throwFailure GUnknownNewsId =
      throwM . RequestedEntityNotFoundException $ toEntityId newsId

data GatewayFailure =
  GUnknownNewsId
