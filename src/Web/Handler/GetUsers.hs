module Web.Handler.GetUsers
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Exception
import qualified Core.Interactor.GetUsers as I
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Web.Exception
import qualified Web.HTTP as Http
import qualified Web.Presenter.UserPresenter as P
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle =
  Handle
    { hGetUsersHandle :: I.Handle IO
    , hPresenterHandle :: P.Handle
    }

run :: Handle -> Wai.Application
run h request respond = do
  pageQuery <- QP.parseQueryM (Wai.queryString request) QP.parsePageQuery
  users <-
    catch
      (I.run (hGetUsersHandle h) pageQuery)
      (throwIO . BadRequestException . queryExceptionReason)
  respond $
    Wai.responseBuilder Http.ok200 [Http.hJSONContentType] $
    P.presentUsers (hPresenterHandle h) users
