module Core.Interactor.GetTag
  ( run
  , Handle(..)
  ) where

import Core.Tag

newtype Handle m =
  Handle
    { hGetTag :: TagId -> m (Maybe Tag)
    }

run :: Handle m -> TagId -> m (Maybe Tag)
run = hGetTag
