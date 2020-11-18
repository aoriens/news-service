module Core.Interactor.GetComment
  ( run
  , Handle(..)
  ) where

import Core.Comment

newtype Handle m =
  Handle
    { hGetComment :: CommentId -> m (Maybe Comment)
    }

run :: Handle m -> CommentId -> m (Maybe Comment)
run Handle {..} = hGetComment
