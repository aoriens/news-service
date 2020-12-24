module Web.Handler.GetTag
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Tag
import Data.Maybe.Util
import Web.Application
import Web.Exception

data Handle m =
  Handle
    { hGetTag :: TagId -> m (Maybe Tag)
    , hPresent :: Tag -> Response
    }

run :: MonadThrow m => Handle m -> TagId -> GenericApplication m
run Handle {..} tagId _ respond = do
  tag <- fromMaybeM (throwM ResourceNotFoundException) =<< hGetTag tagId
  respond $ hPresent tag
