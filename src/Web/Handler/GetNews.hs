module Web.Handler.GetNews
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.News
import Data.Maybe.Util
import Web.Application
import Web.Exception

data Handle m =
  Handle
    { hGetNews :: NewsId -> m (Maybe News)
    , hPresent :: News -> Response
    }

run :: MonadThrow m => Handle m -> NewsId -> GenericApplication m
run Handle {..} newsId _ respond = do
  news <- fromMaybeM (throwM ResourceNotFoundException) =<< hGetNews newsId
  respond $ hPresent news
