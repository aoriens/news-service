module Web.Handler.GetTag
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetTag as IGetTag
import Core.Tag
import Web.Application
import Web.Exception

data Handle =
  Handle
    { hGetTagHandle :: IGetTag.Handle IO
    , hPresenter :: Tag -> Response
    }

run :: Handle -> TagId -> Application
run Handle {..} tagIdent _ respond = do
  tag <-
    maybe (throwIO NotFoundException) pure =<<
    IGetTag.run hGetTagHandle tagIdent
  respond $ hPresenter tag
