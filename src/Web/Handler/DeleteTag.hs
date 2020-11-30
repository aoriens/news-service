module Web.Handler.DeleteTag
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Control.Monad
import Core.Authentication
import Core.Tag
import Web.Application
import qualified Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hDeleteTag :: AuthenticatedUser -> TagId -> IO Bool
    , hAuthenticate :: Maybe Credentials -> IO AuthenticatedUser
    , hPresent :: Response
    }

run :: Handle -> TagId -> Application
run Handle {..} tagId request respond = do
  authUser <-
    hAuthenticate =<< Web.Credentials.getCredentialsFromRequest request
  isOk <- hDeleteTag authUser tagId
  unless isOk $ throwIO NotFoundException
  respond hPresent
