-- | The module introduces some types related to 'Core.Authorization'
-- module. It allows breaking dependency cycles between modules.
module Core.Permission
  ( Permission(..)
  ) where

import Core.Author
import Core.Deletable
import Core.User

data Permission
  = AdminPermission
  | AuthorshipPermission (Deletable AuthorId)
  | AdminOrSpecificUserPermission UserId
  deriving (Eq, Show)
