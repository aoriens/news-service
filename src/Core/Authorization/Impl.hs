module Core.Authorization.Impl
  ( new
  ) where

import Core.Authorization

new :: AuthorizationHandle
new = AuthorizationHandle hasPermission

hasPermission :: Permission -> AuthenticatedUser -> Bool
hasPermission AdminPermission = isAdmin

isAdmin :: AuthenticatedUser -> Bool
isAdmin (IdentifiedUser _ admin _) = admin
isAdmin AnonymousUser = False
