module Core.Interactor.DeleteComment
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.Comment
import Core.User

data Handle m =
  Handle
    { hGetCommentAuthor :: CommentId -> m (Maybe (CommentAuthor UserId))
    -- ^ Nothing means that the comment is not found
    , hDeleteComment :: CommentId -> m Success
    }

type Success = Bool

run :: MonadThrow m => Handle m -> AuthenticatedUser -> CommentId -> m Success
run Handle {..} authUser commentId
  | authenticatedUserIsAdmin authUser = hDeleteComment commentId
  | otherwise =
    hGetCommentAuthor commentId >>= \case
      Nothing -> pure False
      Just commentAuthor -> do
        authorizeWith commentAuthor
        hDeleteComment commentId
  where
    authorizeWith commentAuthor =
      authorize "delete a comment" $
      case commentAuthor of
        AnonymousCommentAuthor -> authUserShouldBeAdmin authUser
        DeletedCommentAuthor -> authUserShouldBeAdmin authUser
        UserCommentAuthor userId ->
          authUserShouldBeAdminOrSpecificUser authUser userId
