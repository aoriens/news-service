-- | High-level data source interface for using in the business logic.
module Database
    -- Authors
  ( createAuthor
  , getAuthors
  , getAuthorIdByUserIdIfExactlyOne
  , getAuthor
  , deleteAuthor
  , updateAuthor
  -- * Categories
  , createCategory
  , getCategory
  , getCategories
  , getCategoryIdBySiblingAndName
  , getCategoryIdByParentAndName
  , categoryIdWithParentAndNameExists
  , categoryIsDescendantOf
  , getCategoryName
  , deleteCategoryAndDescendants
  , setCategoryIdToNewsVersionsInCategoryAndDescendantCategories
  , updateCategory
  -- * Images
  , getImage
  -- * News
  , getNewsList
  , getNews
  , getDraftAuthor
  , getDraftAuthorAndNewsIdItWasCreatedFrom
  , getDraftsOfAuthor
  , getDraftsOfUser
  , getDraftsCreatedFromNewsId
  , getDraft
  , getNewsAuthorId
  , makeDraftIntoNews
  , overwriteNewsWithDraft
  , updateDraft
  , createDraft
  , copyDraftFromNews
  , deleteDraftAndItsContent
  , deleteDraftsOfAuthor
  -- * Tags
  , findTagNamed
  , getTag
  , getTags
  , deleteTag
  , setTagName
  , createTagNamed
  -- * Users
  , createUser
  , getExistingUser
  , getUsers
  , getUserAuthData
  , deleteUser
  -- * Comments
  , createComment
  , getComment
  , getCommentsForNews
  , getCommentAuthor
  , deleteComment
  ) where

import Database.Logic.Authors
import Database.Logic.Categories
import Database.Logic.Comments
import Database.Logic.Images
import Database.Logic.News.Create
import Database.Logic.News.Delete
import Database.Logic.News.Get
import Database.Logic.Tags
import Database.Logic.Users
