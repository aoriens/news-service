module Core.Interactor.CreateTag
  ( run
  , Handle(..)
  , Result(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.Tag
import qualified Data.Text as T

data Handle m =
  Handle
    { hAuthenticationHandle :: AuthenticationHandle m
    , hCreateTagNamed :: T.Text -> m Tag
    , hFindTagByName :: T.Text -> m (Maybe Tag)
    , hAuthorizationHandle :: AuthorizationHandle
    }

data Result
  = TagCreated Tag
  | ExistingTagFound Tag
  deriving (Show, Eq)

run :: MonadThrow m => Handle m -> Maybe Credentials -> T.Text -> m Result
run Handle {..} credentials newTagName = do
  actor <- authenticate hAuthenticationHandle credentials
  requireAdminPermission hAuthorizationHandle actor "create a tag"
  optExistingTag <- hFindTagByName newTagName
  case optExistingTag of
    Just tag -> pure $ ExistingTagFound tag
    Nothing -> TagCreated <$> hCreateTagNamed newTagName
