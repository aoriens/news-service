module Core.Interactor.CreateTag
  ( run
  , Handle(..)
  , Result(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Authorization
import Core.Exception
import Core.Tag
import qualified Data.Text as T

data Handle m =
  Handle
    { hCreateTagNamed :: T.Text -> m Tag
    , hFindTagByName :: T.Text -> m (Maybe Tag)
    , hAuthorizationHandle :: AuthorizationHandle
    }

data Result
  = TagCreated Tag
  | ExistingTagFound Tag
  deriving (Show, Eq)

run :: MonadThrow m => Handle m -> AuthenticatedUser -> T.Text -> m Result
run Handle {..} authUser newTagName = do
  requireAdminPermission hAuthorizationHandle authUser "create a tag"
  when (T.null newTagName) $
    throwM (QueryException "The tag name must not be empty")
  optExistingTag <- hFindTagByName newTagName
  case optExistingTag of
    Just tag -> pure $ ExistingTagFound tag
    Nothing -> TagCreated <$> hCreateTagNamed newTagName
