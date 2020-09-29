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
    { hAuthenticationHandle :: AuthenticationHandle m
    , hCreateAuthor :: UserId -> T.Text -> m (Either Failure Author)
    , hAuthorizationHandle :: AuthorizationHandle
    }

run ::
     MonadThrow m
  => Handle m
  -> Maybe Credentials
  -> UserId
  -> T.Text
  -> m (Either Failure Author)
run Handle {..} credentials uid description = do
  actor <- authenticate hAuthenticationHandle credentials
  requireAdminPermission hAuthorizationHandle actor "create an author"
  hCreateAuthor uid description

data Failure =
  UnknownUserId
  deriving (Eq, Show)
