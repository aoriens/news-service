module Core.Interactor.CreateAuthor
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.User
import qualified Data.Text as T

data Handle m =
  Handle
    { hAuthHandle :: AuthenticationHandle m
    , hCreateAuthor :: UserId -> T.Text -> m (Either Failure Author)
    }

run ::
     MonadThrow m
  => Handle m
  -> Maybe Credentials
  -> UserId
  -> T.Text
  -> m (Either Failure Author)
run h credentials uid description = do
  actor <- authenticate (hAuthHandle h) credentials
  requireAdminPermission actor "create an author"
  hCreateAuthor h uid description

data Failure =
  UnknownUserId
  deriving (Eq, Show)
