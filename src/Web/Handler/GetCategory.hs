module Web.Handler.GetCategory
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Category
import Data.Maybe.Util
import Web.Application
import Web.Exception

data Handle m =
  Handle
    { hGetCategory :: CategoryId -> m (Maybe Category)
    , hPresent :: Category -> Response
    }

run :: MonadThrow m => Handle m -> CategoryId -> GenericApplication m
run Handle {..} catId _ respond = do
  category <-
    fromMaybeM (throwM ResourceNotFoundException) =<< hGetCategory catId
  respond $ hPresent category
