module Web.Handler.GetComment
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Comment
import qualified Core.Interactor.GetComment as IGetComment
import Web.Application
import Web.Exception

data Handle =
  Handle
    { hGetCommentHandle :: IGetComment.Handle IO
    , hPresenter :: Comment -> Response
    }

run :: Handle -> CommentId -> Application
run Handle {..} commentId _ respond = do
  comment <-
    maybe (throwIO NotFoundException) pure =<<
    IGetComment.run hGetCommentHandle commentId
  respond $ hPresenter comment
