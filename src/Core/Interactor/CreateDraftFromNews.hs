{-# LANGUAGE RankNTypes #-}

module Core.Interactor.CreateDraftFromNews
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.Deletable
import Core.News

data Handle m =
  Handle
    { hGetNewsAuthorId :: NewsId -> m (Maybe (Deletable AuthorId))
    , hCopyDraftFromNews :: NewsId -> m Draft
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> NewsId
  -> m (Either Failure Draft)
run Handle {..} authUser newsId =
  hGetNewsAuthorId newsId >>= \case
    Nothing -> pure $ Left UnknownNewsId
    Just authorId -> do
      authorize "create a draft from news" $
        authUser `authUserShouldBeDeletableAuthor` authorId
      Right <$> hCopyDraftFromNews newsId

data Failure =
  UnknownNewsId
  deriving (Eq, Show)
