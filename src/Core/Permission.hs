-- | The module introduces some types related to 'Core.Authorization'
-- module. It allows breaking dependency cycles between modules.
module Core.Permission
  ( Permission(..)
  ) where

data Permission =
  AdminPermission
  deriving (Eq, Show)
