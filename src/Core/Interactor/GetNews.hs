module Core.Interactor.GetNews
  ( run
  , Handle(..)
  ) where

import Core.News

newtype Handle m =
  Handle
    { hGetNews :: NewsId -> m (Maybe News)
    }

run :: Handle m -> NewsId -> m (Maybe News)
run = hGetNews
