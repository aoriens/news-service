module Core.Authorization.Impl
  ( new
  ) where

import Core.Authorization

new :: AuthorizationHandle
new = AuthorizationHandle hasPermission

hasPermission :: Permission -> AuthenticatedUser -> Bool
hasPermission AdminPermission (IdentifiedUser _ isAdmin _) = isAdmin
hasPermission AdminPermission AnonymousUser = False
hasPermission (AuthorshipPermission requiredAuthorId) (IdentifiedUser _ _ authorIds) =
  requiredAuthorId `elem` authorIds
hasPermission (AuthorshipPermission _) AnonymousUser = False
