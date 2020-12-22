module Web.Handler.DeleteComment
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Control.Monad
import Core.Authentication
import Core.Comment
import Web.Application
import qualified Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hDeleteComment :: AuthenticatedUser -> CommentId -> IO Bool
    , hAuthenticate :: Maybe Credentials -> IO AuthenticatedUser
    , hPresent :: Response
    }

run :: Handle -> CommentId -> Application
run Handle {..} commentId request respond = do
  authUser <-
    hAuthenticate =<< Web.Credentials.getCredentialsFromRequest request
  isOk <- hDeleteComment authUser commentId
  unless isOk $ throwIO ResourceNotFoundException
  respond hPresent
