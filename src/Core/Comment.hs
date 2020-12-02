{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Comment
  ( Comment(..)
  , CommentId(..)
  , CommentAuthor(..)
  ) where

import Core.News
import Core.User
import Data.Hashable
import Data.Int
import qualified Data.Text as T
import Data.Time

data Comment =
  Comment
    { commentId :: CommentId
    , commentNewsId :: NewsId
    , commentAuthor :: CommentAuthor User
    , commentCreatedAt :: UTCTime
    , commentText :: T.Text
    }
  deriving (Eq, Show)

newtype CommentId =
  CommentId
    { getCommentId :: Int32
    }
  deriving (Eq, Show, Hashable)

-- | The comment author. It is not a 'Maybe', since nested Maybes look
-- confusing, and the type can be extended. It is parameterized with a
-- user type which can be either UserId or User.
data CommentAuthor userType
  = UserCommentAuthor userType
  | AnonymousCommentAuthor
  | DeletedCommentAuthor
  deriving (Eq, Show)

instance Functor CommentAuthor where
  fmap _ AnonymousCommentAuthor = AnonymousCommentAuthor
  fmap _ DeletedCommentAuthor = DeletedCommentAuthor
  fmap f (UserCommentAuthor u) = UserCommentAuthor (f u)
