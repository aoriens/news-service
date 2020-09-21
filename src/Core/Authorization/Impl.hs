module Core.Authorization.Impl
  ( new
  ) where

import Core.Authorization

new :: AuthorizationHandle
new = AuthorizationHandle $ \AdminPermission -> isAdmin

isAdmin :: AuthenticatedUser -> Bool
isAdmin (IdentifiedUser _ admin) = admin
isAdmin _ = False
