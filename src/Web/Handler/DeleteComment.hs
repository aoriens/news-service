module Web.Handler.DeleteComment
  ( run
  , Handle(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Authentication
import Core.Comment
import Web.Application
import qualified Web.Credentials
import Web.Exception

data Handle m =
  Handle
    { hDeleteComment :: AuthenticatedUser -> CommentId -> m Bool
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    , hPresent :: Response
    }

run :: MonadThrow m => Handle m -> CommentId -> GenericApplication m
run Handle {..} commentId request respond = do
  authUser <-
    hAuthenticate =<< Web.Credentials.getCredentialsFromRequest request
  isOk <- hDeleteComment authUser commentId
  unless isOk $ throwM ResourceNotFoundException
  respond hPresent
