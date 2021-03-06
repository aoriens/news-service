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

newtype Handle m =
  Handle
    { hCreateAuthor :: UserId -> T.Text -> m (Either Failure Author)
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> UserId
  -> T.Text
  -> m (Either Failure Author)
run Handle {..} authUser uid description = do
  authorize "create an author" $ authUserShouldBeAdmin authUser
  hCreateAuthor uid description

data Failure =
  UnknownUserId
  deriving (Eq, Show)
