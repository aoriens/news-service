module Database
  -- * Authors
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
  , getDraftsOfAuthor
  , getDraftsOfUser
  , getDraft
  , createNews
  , createNewsVersion
  , deleteNewsVersion
  , deleteDraftsOfAuthor
  -- * Tags
  , findTagNamed
  , findTagById
  , getTags
  , deleteTag
  , setTagName
  , createTagNamed
  -- * Users
  , createUser
  , getUser
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

import Core.Authentication.Impl
import Core.Author
import Core.Category
import Core.Comment
import Core.Deletable
import Core.Image
import qualified Core.Interactor.CreateAuthor as ICreateAuthor
import qualified Core.Interactor.CreateCategory as ICreateCategory
import qualified Core.Interactor.CreateComment as ICreateComment
import qualified Core.Interactor.CreateDraft as ICreateDraft
import qualified Core.Interactor.CreateUser as ICreateUser
import qualified Core.Interactor.DeleteAuthor as IDeleteAuthor
import qualified Core.Interactor.GetCommentsForNews as IGetCommentsForNews
import qualified Core.Interactor.GetNewsList as IListNews
import qualified Core.Interactor.UpdateCategory as IUpdateCategory
import qualified Core.Interactor.UpdateTag as IUpdateTag
import Core.News
import Core.Pagination
import Core.Tag
import Core.User
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
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
import qualified Database.Logic.News.GetDraftAuthor as DNews
import qualified Database.Logic.Tags as DTags
import qualified Database.Logic.Users as DUsers
import Database.Service.Primitives as DB

createAuthor ::
     DB.Handle -> UserId -> T.Text -> IO (Either ICreateAuthor.Failure Author)
createAuthor h uid description =
  runTransactionRW h $ DAuthors.createAuthor uid description

getAuthors :: DB.Handle -> PageSpec -> IO [Author]
getAuthors h page = toList <$> runTransactionRO h (DAuthors.selectAuthors page)

getAuthorIdByUserIdIfExactlyOne :: DB.Handle -> UserId -> IO (Maybe AuthorId)
getAuthorIdByUserIdIfExactlyOne h =
  runTransactionRO h . DAuthors.selectAuthorIdByUserIdIfExactlyOne

getAuthor :: DB.Handle -> AuthorId -> IO (Maybe Author)
getAuthor h authorId' = runTransactionRO h (DAuthors.selectAuthorById authorId')

deleteAuthor :: DB.Handle -> AuthorId -> IO (Either IDeleteAuthor.Failure ())
deleteAuthor h = runTransactionRW h . DAuthors.deleteAuthor

updateAuthor :: DB.Handle -> AuthorId -> T.Text -> IO (Maybe Author)
updateAuthor h aid newDescription =
  runTransactionRW h $ do
    DAuthors.updateAuthor aid newDescription
    DAuthors.selectAuthorById aid

createCategory ::
     DB.Handle
  -> Maybe CategoryId
  -> NonEmpty Text
  -> IO (Either ICreateCategory.CreateCategoryFailure Category)
createCategory h parentId =
  runTransactionRW h . DCategories.createCategory parentId

getCategory :: DB.Handle -> CategoryId -> IO (Maybe Category)
getCategory h = runTransactionRO h . DCategories.selectCategory

getCategories :: DB.Handle -> PageSpec -> IO [Category]
getCategories h = runTransactionRO h . DCategories.selectCategories

deleteCategoryAndDescendants :: DB.Handle -> CategoryId -> IO ()
deleteCategoryAndDescendants h =
  runTransactionRW h . DCategories.deleteCategoryAndDescendants

setCategoryIdToNewsVersionsInCategoryAndDescendantCategories ::
     DB.Handle -> Maybe CategoryId -> CategoryId -> IO ()
setCategoryIdToNewsVersionsInCategoryAndDescendantCategories h newCatId oldCatId =
  runTransactionRW h $
  DCategories.setCategoryIdToNewsVersionsInCategoryAndDescendantCategories
    newCatId
    oldCatId

updateCategory ::
     DB.Handle
  -> IUpdateCategory.Request
  -> IO (Either IUpdateCategory.UpdateCategoryFailure Category)
updateCategory h = runTransactionRW h . DCategories.updateCategory

getCategoryIdBySiblingAndName ::
     DB.Handle -> CategoryId -> T.Text -> IO (Maybe CategoryId)
getCategoryIdBySiblingAndName h catId name =
  runTransactionRO h $ DCategories.getCategoryIdBySiblingAndName catId name

getCategoryIdByParentAndName ::
     DB.Handle -> Maybe CategoryId -> T.Text -> IO (Maybe CategoryId)
getCategoryIdByParentAndName h catId name =
  runTransactionRO h $ DCategories.getCategoryIdByParentAndName catId name

categoryIdWithParentAndNameExists ::
     DB.Handle -> Maybe CategoryId -> T.Text -> IO Bool
categoryIdWithParentAndNameExists h parentId name =
  isJust <$> getCategoryIdByParentAndName h parentId name

categoryIsDescendantOf :: DB.Handle -> CategoryId -> CategoryId -> IO Bool
categoryIsDescendantOf h desc anc =
  runTransactionRO h $ DCategories.categoryIsDescendantOf desc anc

getCategoryName :: DB.Handle -> CategoryId -> IO (Maybe T.Text)
getCategoryName h = runTransactionRO h . DCategories.getCategoryName

getImage :: DB.Handle -> ImageId -> IO (Maybe Image)
getImage h = DB.runTransactionRO h . DImages.selectImage

getNewsList ::
     DB.Handle
  -> IListNews.GatewayFilter
  -> IListNews.SortOptions
  -> PageSpec
  -> IO [News]
getNewsList h nf sortOptions =
  DB.runTransactionRO h . DNews.getNewsList nf sortOptions

getNews :: DB.Handle -> NewsId -> IO (Maybe News)
getNews h = DB.runTransactionRO h . DNews.getNews

createNewsVersion ::
     DB.Handle
  -> ICreateDraft.CreateNewsVersionCommand
  -> IO (Either ICreateDraft.GatewayFailure NewsVersion)
createNewsVersion h = DB.runTransactionRW h . DNews.createNewsVersion

getDraftAuthor :: DB.Handle -> NewsVersionId -> IO (Maybe (Deletable AuthorId))
getDraftAuthor h = DB.runTransactionRO h . DNews.getDraftAuthor

getDraftsOfAuthor :: DB.Handle -> AuthorId -> PageSpec -> IO [NewsVersion]
getDraftsOfAuthor h authorId pageSpec =
  DB.runTransactionRO h $ DNews.getDraftsOfAuthor authorId pageSpec

getDraftsOfUser :: DB.Handle -> UserId -> PageSpec -> IO [NewsVersion]
getDraftsOfUser h userId pageSpec =
  DB.runTransactionRO h $ DNews.getDraftsOfUser userId pageSpec

getDraft :: DB.Handle -> NewsVersionId -> IO (Maybe NewsVersion)
getDraft h = DB.runTransactionRO h . DNews.getDraft

deleteNewsVersion :: DB.Handle -> NewsVersionId -> IO ()
deleteNewsVersion h = DB.runTransactionRW h . DNews.deleteNewsVersion

deleteDraftsOfAuthor :: DB.Handle -> AuthorId -> IO ()
deleteDraftsOfAuthor h = DB.runTransactionRW h . DNews.deleteDraftsOfAuthor

createNews :: DB.Handle -> NewsVersionId -> Day -> IO News
createNews h vId day = DB.runTransactionRW h $ DNews.createNews vId day

findTagNamed :: DB.Handle -> Text -> IO (Maybe Tag)
findTagNamed h = runTransactionRO h . DTags.findTagNamed

findTagById :: DB.Handle -> TagId -> IO (Maybe Tag)
findTagById h = runTransactionRO h . DTags.findTagById

getTags :: DB.Handle -> PageSpec -> IO [Tag]
getTags h = runTransactionRO h . DTags.getTags

deleteTag :: DB.Handle -> TagId -> IO Bool
deleteTag h = runTransactionRW h . DTags.deleteTag

createTagNamed :: DB.Handle -> Text -> IO Tag
createTagNamed h = runTransactionRW h . DTags.createTagNamed

setTagName ::
     DB.Handle -> TagId -> Text -> IO (Either IUpdateTag.SetTagNameFailure ())
setTagName h tagId newName = runTransactionRW h $ DTags.setTagName tagId newName

createUser ::
     DB.Handle
  -> ICreateUser.CreateUserCommand
  -> IO ICreateUser.CreateUserResult
createUser h = runTransactionRW h . DUsers.createUser

getUser :: DB.Handle -> UserId -> IO (Maybe User)
getUser h = runTransactionRO h . DUsers.getExistingUser

getUsers :: DB.Handle -> PageSpec -> IO [User]
getUsers h page = toList <$> runTransactionRO h (DUsers.selectUsers page)

getUserAuthData :: DB.Handle -> UserId -> IO (Maybe UserAuthData)
getUserAuthData h = runTransactionRO h . DUsers.selectUserAuthData

deleteUser :: DB.Handle -> UserId -> IO Bool
deleteUser h = runTransactionRW h . DUsers.deleteUser

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
