module Core.Authorization.Impl
  ( new
  ) where

import Core.Authorization

new :: AuthorizationHandle
new = AuthorizationHandle $ \AdminPermission -> isAdmin

isAdmin :: AuthenticatedUser -> Bool
isAdmin (IdentifiedUser _ admin _) = admin
isAdmin _ = False
