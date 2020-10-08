module Web.Handler.GetTags
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.GetTags as IGetTags
import Core.Tag
import Web.Application
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle =
  Handle
    { hGetTagsHandle :: IGetTags.Handle IO
    , hPresenter :: [Tag] -> Response
    }

run :: Handle -> Application
run Handle {..} request respond = do
  pageQuery <- QP.parseQueryM (requestQueryString request) QP.parsePageQuery
  tags <- IGetTags.run hGetTagsHandle pageQuery
  respond $ hPresenter tags
