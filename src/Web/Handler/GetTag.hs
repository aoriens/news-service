module Web.Handler.GetTag
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetTag as IGetTag
import Core.Tag
import Data.Maybe.Util
import Web.Application
import Web.Exception

data Handle =
  Handle
    { hGetTagHandle :: IGetTag.Handle IO
    , hPresent :: Tag -> Response
    }

run :: Handle -> TagId -> Application
run Handle {..} tagId' _ respond = do
  tag <-
    fromMaybeM (throwIO NotFoundException) =<< IGetTag.run hGetTagHandle tagId'
  respond $ hPresent tag
