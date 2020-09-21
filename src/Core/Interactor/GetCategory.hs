module Core.Interactor.GetCategory
  ( run
  , Handle(..)
  ) where

import Core.Category

newtype Handle m =
  Handle
    { hGetCategory :: CategoryId -> m (Maybe Category)
    }

run :: Handle m -> CategoryId -> m (Maybe Category)
run = hGetCategory
