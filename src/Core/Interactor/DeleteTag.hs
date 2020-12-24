module Core.Interactor.DeleteTag
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.Tag

newtype Handle m =
  Handle
    { hDeleteTag :: TagId -> m Success
    }

type Success = Bool

run :: MonadThrow m => Handle m -> AuthenticatedUser -> TagId -> m Success
run Handle {..} authUser tagId = do
  authorize "delete a tag" $ authUserShouldBeAdmin authUser
  hDeleteTag tagId
