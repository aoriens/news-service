module Core.Interactor.GetUser
  ( run
  , Handle(..)
  ) where

import Core.User

newtype Handle m =
  Handle
    { hGetUser :: UserId -> m (Maybe User)
    }

run :: Handle m -> UserId -> m (Maybe User)
run = hGetUser
