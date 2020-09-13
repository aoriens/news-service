module Web.Handler.GetUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetUser as I
import qualified Network.Wai as Wai
import Web.Exception
import Web.PathParameter
import Web.Representation.User
import Web.RepresentationBuilder

data Handle =
  Handle
    { hGetUserHandle :: I.Handle IO
    , hPresenterHandle :: RepBuilderHandle
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  userIdent <-
    maybe (throwIO NotFoundException) pure $
    userIdFromPath (Wai.pathInfo request)
  user <-
    maybe (throwIO NotFoundException) pure =<< I.run hGetUserHandle userIdent
  respond $ runRepBuilder hPresenterHandle $ userRepresentation Nothing user
