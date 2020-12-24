module Core.Interactor.DeleteUser
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.User

newtype Handle m =
  Handle
    { hDeleteUser :: UserId -> m Success
    -- ^ Returns 'False' when no such existing user is found.
    }

type Success = Bool

-- | Returns 'False' when no such existing user is found.
run :: MonadThrow m => Handle m -> AuthenticatedUser -> UserId -> m Success
run Handle {..} authUser userId = do
  authorize "delete a user" $ authUserShouldBeAdmin authUser
  hDeleteUser userId
