-- | High-level data source interface for using in the business logic.
module Database
   -- Database access
  ( initialize
  , Handle(..)
  , Transaction
  , runTransactionRW
   -- * Authors
  , createAuthor
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

import qualified Data.Text as T
import Database.Logic.Authors
import Database.Logic.Categories
import Database.Logic.Comments
import Database.Logic.Images
import Database.Logic.News.Create
import Database.Logic.News.Delete
import Database.Logic.News.Get
import Database.Logic.Tags
import Database.Logic.Users
import qualified Database.Service.SchemaVersion as SchemaVersion
import Database.Service.Transaction
import qualified Logger
import System.Exit

-- | It must be invoked before the first access to the database.
-- However, it would be better to use some 'InitializationResult'
-- singleton type to enforce invoking 'initialize' before
-- 'runTransactionRW' from outside.
initialize :: Handle -> SchemaVersion.MigrationsDirectoryPath -> IO ()
initialize h migrationsPath =
  SchemaVersion.check h migrationsPath >>= \case
    Right () -> pure ()
    Left failure -> dieWithSchemaVersionFailure failure
  where
    dieWithSchemaVersionFailure f = do
      Logger.error (hLoggerHandle h) . T.pack $ reason f
      die $ reason f
    reason f = "Database migration check failed: " ++ show f
