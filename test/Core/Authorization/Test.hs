module Core.Authorization.Test
  ( noOpAuthorizationHandle
  ) where

import Core.Authorization

noOpAuthorizationHandle :: AuthorizationHandle
noOpAuthorizationHandle = AuthorizationHandle $ \_ _ -> True
