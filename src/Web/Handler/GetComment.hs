module Web.Handler.GetComment
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Comment
import qualified Core.Interactor.GetComment as IGetComment
import Data.Maybe.Util
import Web.Application
import Web.Exception

data Handle =
  Handle
    { hGetCommentHandle :: IGetComment.Handle IO
    , hPresent :: Comment -> Response
    }

run :: Handle -> CommentId -> Application
run Handle {..} commentId _ respond = do
  comment <-
    fromMaybeM (throwIO NotFoundException) =<<
    IGetComment.run hGetCommentHandle commentId
  respond $ hPresent comment
