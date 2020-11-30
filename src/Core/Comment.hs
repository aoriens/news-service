module Core.Comment
  ( Comment(..)
  , CommentId(..)
  , CommentAuthor(..)
  ) where

import Core.News
import Core.User
import Data.Int
import qualified Data.Text as T
import Data.Time

data Comment =
  Comment
    { commentId :: CommentId
    , commentNewsId :: NewsId
    , commentAuthor :: CommentAuthor User
    -- ^ When the author is Nothing, the comment is posted anonymously
    , commentCreatedAt :: UTCTime
    , commentText :: T.Text
    }
  deriving (Eq, Show)

newtype CommentId =
  CommentId
    { getCommentId :: Int32
    }
  deriving (Eq, Show)

-- | The comment author. It is not a 'Maybe', since nested Maybes look
-- confusing, and the type can be extended. It is parameterized with a
-- user type which can be either UserId or User.
data CommentAuthor userType
  = UserCommentAuthor userType
  | AnonymousCommentAuthor
  deriving (Eq, Show)
