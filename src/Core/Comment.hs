module Core.Comment
  ( Comment(..)
  , CommentId(..)
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
    , commentAuthor :: Maybe User
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
