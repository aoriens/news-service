module Core.Stubs
  ( stubNews
  , stubNewsVersion
  , stubAuthor
  , stubUser
  , stubCategory
  , stubComment
  , stubUTCTime
  , stubDay
  ) where

import Core.Author
import Core.Category
import Core.Comment
import Core.Deletable
import Core.News
import Core.User
import qualified Data.HashSet as Set
import Data.Time

stubNews :: News
stubNews =
  News {newsId = NewsId 0, newsDate = stubDay, newsVersion = stubNewsVersion}

stubNewsVersion :: NewsVersion
stubNewsVersion =
  NewsVersion
    { nvId = NewsVersionId 0
    , nvTitle = ""
    , nvText = ""
    , nvAuthor = Existing stubAuthor
    , nvCategory = stubCategory
    , nvTags = Set.empty
    , nvAdditionalPhotoIds = Set.empty
    , nvMainPhotoId = Nothing
    }

stubAuthor :: Author
stubAuthor =
  Author
    { authorId = AuthorId 0
    , authorUser = Existing stubUser
    , authorDescription = ""
    }

stubUser :: User
stubUser =
  User
    { userId = UserId 0
    , userFirstName = Nothing
    , userLastName = ""
    , userAvatarId = Nothing
    , userCreatedAt = stubUTCTime
    , userIsAdmin = False
    }

stubUTCTime :: UTCTime
stubUTCTime = UTCTime stubDay 0

stubDay :: Day
stubDay = ModifiedJulianDay 0

stubCategory :: Category
stubCategory =
  Category
    {categoryId = CategoryId 0, categoryName = "", categoryParent = Nothing}

stubComment :: Comment
stubComment =
  Comment
    { commentId = CommentId 0
    , commentNewsId = NewsId 0
    , commentAuthor = AnonymousCommentAuthor
    , commentCreatedAt = UTCTime (ModifiedJulianDay 0) 0
    , commentText = ""
    }
