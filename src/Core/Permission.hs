-- | The module introduces some types related to 'Core.Authorization'
-- module. It allows breaking dependency cycles between modules.
module Core.Permission
  ( Permission(..)
  ) where

import Core.Author

data Permission
  = AdminPermission
  | AuthorshipPermission AuthorId
  deriving (Eq, Show)
