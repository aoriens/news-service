module Web.Handler.GetComment
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Comment
import Data.Maybe.Util
import Web.Application
import Web.Exception

data Handle m =
  Handle
    { hGetComment :: CommentId -> m (Maybe Comment)
    , hPresent :: Comment -> Response
    }

run :: MonadThrow m => Handle m -> CommentId -> GenericApplication m
run Handle {..} commentId _ respond = do
  comment <-
    fromMaybeM (throwM ResourceNotFoundException) =<< hGetComment commentId
  respond $ hPresent comment
