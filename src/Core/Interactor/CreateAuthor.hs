module Core.Interactor.CreateAuthor
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import qualified Core.Authorization as A
import Core.User
import qualified Data.Text as T

data Handle m =
  Handle
    { hAuthHandle :: A.Handle m
    , hCreateAuthor :: UserId -> T.Text -> m (Either Failure Author)
    }

run ::
     MonadThrow m
  => Handle m
  -> Maybe A.Credentials
  -> UserId
  -> T.Text
  -> m (Either Failure Author)
run h credentials uid description = do
  actor <- A.authenticate (hAuthHandle h) credentials
  A.requiresAdminPermission actor "create an author" $
    hCreateAuthor h uid description

data Failure =
  UnknownUserId
