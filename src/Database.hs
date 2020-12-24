-- | High-level data source interface for using in the business logic.
module Database
  -- * Authors
  ( DAuthors.createAuthor
  , DAuthors.getAuthors
  , getAuthorIdByUserIdIfExactlyOne
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
  , DNews.deleteDraftsOfAuthor
  -- * Tags
  , findTagNamed
  , getTag
  , getTags
  , deleteTag
  , setTagName
  , createTagNamed
  -- * Users
  , DUsers.createUser
  , DUsers.getExistingUser
  , DUsers.getUsers
  , getUserAuthData
  , DUsers.deleteUser
  -- * Comments
  , createComment
  , getComment
  , getCommentsForNews
  , getCommentAuthor
  , deleteComment
  ) where

import Core.Authentication.Impl
import Core.Author
import Core.Comment
import Core.Deletable
import qualified Core.Interactor.CreateComment as ICreateComment
import qualified Core.Interactor.CreateDraft as ICreateDraft
import qualified Core.Interactor.GetCommentsForNews as IGetCommentsForNews
import qualified Core.Interactor.PublishDraft as IPublishDraft
import qualified Core.Interactor.UpdateDraft as IUpdateDraft
import qualified Core.Interactor.UpdateTag as IUpdateTag
import Core.News
import Core.Pagination
import Core.Tag
import Core.User
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Database.Logic.Authors as DAuthors
import qualified Database.Logic.Categories as DCategories
import qualified Database.Logic.Comments as DComments
import qualified Database.Logic.Images as DImages
import qualified Database.Logic.News.Create as DNews
import qualified Database.Logic.News.Delete as DNews
import qualified Database.Logic.News.Get as DNews
import qualified Database.Logic.Tags as DTags
import qualified Database.Logic.Users as DUsers
import Database.Service.Primitives as DB

getAuthorIdByUserIdIfExactlyOne :: DB.Handle -> UserId -> IO (Maybe AuthorId)
getAuthorIdByUserIdIfExactlyOne h =
  runTransactionRO h . DAuthors.getAuthorIdByUserIdIfExactlyOne

createDraft ::
     DB.Handle
  -> ICreateDraft.CreateDraftCommand
  -> IO (Either ICreateDraft.CreateDraftFailure Draft)
createDraft h = DB.runTransactionRW h . DNews.createDraft

updateDraft ::
     DB.Handle
  -> DraftId
  -> IUpdateDraft.UpdateDraftRequest
  -> IO (Either IUpdateDraft.UpdateDraftFailure Draft)
updateDraft h draftId request =
  DB.runTransactionRW h $ DNews.updateDraft draftId request

copyDraftFromNews :: DB.Handle -> NewsId -> IO Draft
copyDraftFromNews h = DB.runTransactionRW h . DNews.copyDraftFromNews

getDraftAuthor :: DB.Handle -> DraftId -> IO (Maybe (Deletable AuthorId))
getDraftAuthor h = DB.runTransactionRO h . DNews.getDraftAuthor

getDraftAuthorAndNewsIdItWasCreatedFrom ::
     DB.Handle -> DraftId -> IO (Maybe (Deletable AuthorId, Maybe NewsId))
getDraftAuthorAndNewsIdItWasCreatedFrom h =
  DB.runTransactionRO h . DNews.getDraftAuthorAndNewsIdItWasCreatedFrom

getDraftsOfAuthor :: DB.Handle -> AuthorId -> PageSpec -> IO [Draft]
getDraftsOfAuthor h authorId pageSpec =
  DB.runTransactionRO h $ DNews.getDraftsOfAuthor authorId pageSpec

getDraftsOfUser :: DB.Handle -> UserId -> PageSpec -> IO [Draft]
getDraftsOfUser h userId pageSpec =
  DB.runTransactionRO h $ DNews.getDraftsOfUser userId pageSpec

getDraftsCreatedFromNewsId :: DB.Handle -> NewsId -> PageSpec -> IO [Draft]
getDraftsCreatedFromNewsId h newsId pageSpec =
  DB.runTransactionRO h $ DNews.getDraftsCreatedFromNewsId newsId pageSpec

getDraft :: DB.Handle -> DraftId -> IO (Maybe Draft)
getDraft h = DB.runTransactionRO h . DNews.getDraft

getNewsAuthorId :: DB.Handle -> NewsId -> IO (Maybe (Deletable AuthorId))
getNewsAuthorId h = DB.runTransactionRO h . DNews.getNewsAuthorId

deleteDraftAndItsContent :: DB.Handle -> DraftId -> IO ()
deleteDraftAndItsContent h =
  DB.runTransactionRW h . DNews.deleteDraftAndItsContent

makeDraftIntoNews ::
     DB.Handle
  -> DraftId
  -> Day
  -> IO (Either IPublishDraft.MakeDraftIntoNewsFailure News)
makeDraftIntoNews h vId day =
  DB.runTransactionRW h $ DNews.makeDraftIntoNews vId day

overwriteNewsWithDraft ::
     DB.Handle
  -> NewsId
  -> DraftId
  -> Day
  -> IO (Either IPublishDraft.OverwriteNewsWithDraftFailure News)
overwriteNewsWithDraft h newsId draftId day =
  DB.runTransactionRW h $ DNews.overwriteNewsWithDraft newsId draftId day

findTagNamed :: DB.Handle -> Text -> IO (Maybe Tag)
findTagNamed h = runTransactionRO h . DTags.findTagNamed

getTag :: DB.Handle -> TagId -> IO (Maybe Tag)
getTag h = runTransactionRO h . DTags.getTag

getTags :: DB.Handle -> PageSpec -> IO [Tag]
getTags h = runTransactionRO h . DTags.getTags

deleteTag :: DB.Handle -> TagId -> IO Bool
deleteTag h = runTransactionRW h . DTags.deleteTag

createTagNamed :: DB.Handle -> Text -> IO Tag
createTagNamed h = runTransactionRW h . DTags.createTagNamed

setTagName ::
     DB.Handle -> TagId -> Text -> IO (Either IUpdateTag.SetTagNameFailure ())
setTagName h tagId newName = runTransactionRW h $ DTags.setTagName tagId newName

getUserAuthData :: DB.Handle -> UserId -> IO (Maybe UserAuthData)
getUserAuthData h = runTransactionRO h . DUsers.getUserAuthData

createComment ::
     DB.Handle
  -> T.Text
  -> Maybe UserId
  -> NewsId
  -> UTCTime
  -> IO (Either ICreateComment.GatewayFailure Comment)
createComment h text optUserId newsId' time =
  runTransactionRW h $ DComments.createComment text optUserId newsId' time

getComment :: DB.Handle -> CommentId -> IO (Maybe Comment)
getComment h = runTransactionRO h . DComments.getComment

getCommentsForNews ::
     DB.Handle
  -> NewsId
  -> PageSpec
  -> IO (Either IGetCommentsForNews.GatewayFailure [Comment])
getCommentsForNews h newsId pageSpec =
  runTransactionRO h $ DComments.getCommentsForNews newsId pageSpec

getCommentAuthor :: DB.Handle -> CommentId -> IO (Maybe (CommentAuthor UserId))
getCommentAuthor h = runTransactionRO h . DComments.getCommentAuthor

deleteComment :: DB.Handle -> CommentId -> IO Bool
deleteComment h = runTransactionRW h . DComments.deleteComment
