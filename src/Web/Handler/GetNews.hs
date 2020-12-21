module Web.Handler.GetNews
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetNews as I
import Core.News
import Data.Maybe.Util
import Web.Application
import Web.Exception

data Handle =
  Handle
    { hGetNewsHandle :: I.Handle IO
    , hPresent :: News -> Response
    }

run :: Handle -> NewsId -> Application
run Handle {..} newsId _ respond = do
  news <- fromMaybeM (throwIO NotFoundException) =<< I.run hGetNewsHandle newsId
  respond $ hPresent news
