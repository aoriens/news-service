module Web.Handler.GetUsers
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.GetUsers as I
import qualified Network.Wai as Wai
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP
import Web.Representation.User
import Web.RepresentationBuilder

data Handle =
  Handle
    { hGetUsersHandle :: I.Handle IO
    , hPresenterHandle :: RepBuilderHandle
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  pageQuery <- QP.parseQueryM (Wai.queryString request) QP.parsePageQuery
  users <- I.run hGetUsersHandle pageQuery
  respond $
    runRepBuilder hPresenterHandle $ mapM (userRepresentation Nothing) users
