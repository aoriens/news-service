-- | High-level data source interface for using in the business logic.
module Database
  -- * Authors
  ( DAuthors.createAuthor
  , DAuthors.getAuthors
  , DAuthors.getAuthorIdByUserIdIfExactlyOne
  , DAuthors.getAuthor
  , DAuthors.deleteAuthor
  , DAuthors.updateAuthor
  -- * Categories
  , DCategories.createCategory
  , DCategories.getCategory
  , DCategories.getCategories
  , DCategories.getCategoryIdBySiblingAndName
  , DCategories.getCategoryIdByParentAndName
  , DCategories.categoryIdWithParentAndNameExists
  , DCategories.categoryIsDescendantOf
  , DCategories.getCategoryName
  , DCategories.deleteCategoryAndDescendants
  , DCategories.setCategoryIdToNewsVersionsInCategoryAndDescendantCategories
  , DCategories.updateCategory
  -- * Images
  , DImages.getImage
  -- * News
  , DNews.getNewsList
  , DNews.getNews
  , DNews.getDraftAuthor
  , DNews.getDraftAuthorAndNewsIdItWasCreatedFrom
  , DNews.getDraftsOfAuthor
  , DNews.getDraftsOfUser
  , DNews.getDraftsCreatedFromNewsId
  , DNews.getDraft
  , DNews.getNewsAuthorId
  , DNews.makeDraftIntoNews
  , DNews.overwriteNewsWithDraft
  , DNews.updateDraft
  , DNews.createDraft
  , DNews.copyDraftFromNews
  , DNews.deleteDraftAndItsContent
  , DNews.deleteDraftsOfAuthor
  -- * Tags
  , DTags.findTagNamed
  , DTags.getTag
  , DTags.getTags
  , DTags.deleteTag
  , DTags.setTagName
  , DTags.createTagNamed
  -- * Users
  , DUsers.createUser
  , DUsers.getExistingUser
  , DUsers.getUsers
  , DUsers.getUserAuthData
  , DUsers.deleteUser
  -- * Comments
  , DComments.createComment
  , DComments.getComment
  , DComments.getCommentsForNews
  , DComments.getCommentAuthor
  , DComments.deleteComment
  ) where

import qualified Database.Logic.Authors as DAuthors
import qualified Database.Logic.Categories as DCategories
import qualified Database.Logic.Comments as DComments
import qualified Database.Logic.Images as DImages
import qualified Database.Logic.News.Create as DNews
import qualified Database.Logic.News.Delete as DNews
import qualified Database.Logic.News.Get as DNews
import qualified Database.Logic.Tags as DTags
import qualified Database.Logic.Users as DUsers
