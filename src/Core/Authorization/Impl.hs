module Core.Authorization.Impl
  ( new
  ) where

import Core.Authorization
import Core.Deletable

new :: AuthorizationHandle
new = AuthorizationHandle hasPermission

hasPermission :: Permission -> AuthenticatedUser -> Bool
hasPermission AdminPermission (IdentifiedUser _ isAdmin _) = isAdmin
hasPermission AdminPermission AnonymousUser = False
hasPermission (AuthorshipPermission requiredAuthorId) (IdentifiedUser _ _ authorIds) =
  case requiredAuthorId of
    Deleted -> False
    Existing authorId -> authorId `elem` authorIds
hasPermission (AuthorshipPermission _) AnonymousUser = False
hasPermission (AdminOrSpecificUserPermission _) _ = error "Not implemented"
