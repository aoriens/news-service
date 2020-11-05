module Database
  ( createAuthor
  , getAuthors
  , getAuthorIdByUserIdIfExactlyOne
  , getAuthor
  , deleteAuthor
  , updateAuthor
  , createCategory
  , getCategory
  , getCategories
  , deleteCategory
  , getImage
  , getNewsList
  , createNewsVersion
  , getAuthorOfNewsVersion
  , createNews
  , findTagByName
  , findTagById
  , getTags
  , createTagNamed
  , createUser
  , getUser
  , getUsers
  , getUserAuthData
  , deleteUser
  ) where

import Core.Authentication.Impl
import Core.Author
import Core.Category
import Core.Image
import qualified Core.Interactor.CreateAuthor as ICreateAuthor
import qualified Core.Interactor.CreateCategory as ICreateCategory
import qualified Core.Interactor.CreateDraft as ICreateDraft
import qualified Core.Interactor.CreateUser as ICreateUser
import qualified Core.Interactor.DeleteCategory as IDeleteCategory
import qualified Core.Interactor.DeleteUser as IDeleteUser
import qualified Core.Interactor.PublishDraft as IPublishDraft
import Core.News
import Core.Pagination
import Core.Tag
import Core.User
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Database.Logic.Authors as DAuthors
import qualified Database.Logic.Categories as DCategories
import qualified Database.Logic.Images as DImages
import qualified Database.Logic.News as DNews
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

deleteAuthor :: DB.Handle -> AuthorId -> IO Bool
deleteAuthor h = runTransactionRW h . fmap (0 /=) . DAuthors.deleteAuthorById

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

deleteCategory ::
     DB.Handle
  -> CategoryId
  -> PageSpec
  -> IO (Either IDeleteCategory.Failure ())
deleteCategory h = (runTransactionRW h .) . DCategories.deleteCategory

getImage :: DB.Handle -> ImageId -> IO (Maybe Image)
getImage h = DB.runTransactionRO h . DImages.selectImage

getNewsList :: DB.Handle -> PageSpec -> IO [News]
getNewsList h = DB.runTransactionRO h . DNews.getNewsList

createNewsVersion ::
     DB.Handle
  -> ICreateDraft.CreateNewsVersionCommand
  -> IO (Either ICreateDraft.GatewayFailure NewsVersion)
createNewsVersion h = DB.runTransactionRW h . DNews.createNewsVersion

getAuthorOfNewsVersion ::
     DB.Handle
  -> NewsVersionId
  -> IO (Either IPublishDraft.GatewayFailure AuthorId)
getAuthorOfNewsVersion h = DB.runTransactionRO h . DNews.getAuthorOfNewsVersion

createNews :: DB.Handle -> NewsVersionId -> Day -> IO News
createNews h vId day = DB.runTransactionRW h $ DNews.createNews vId day

findTagByName :: DB.Handle -> Text -> IO (Maybe Tag)
findTagByName h = runTransactionRO h . DTags.findTagByName

findTagById :: DB.Handle -> TagId -> IO (Maybe Tag)
findTagById h = runTransactionRO h . DTags.findTagById

getTags :: DB.Handle -> PageSpec -> IO [Tag]
getTags h = runTransactionRO h . DTags.getTags

createTagNamed :: DB.Handle -> Text -> IO Tag
createTagNamed h = runTransactionRW h . DTags.createTagNamed

createUser ::
     DB.Handle
  -> ICreateUser.CreateUserCommand
  -> IO ICreateUser.CreateUserResult
createUser h = runTransactionRW h . DUsers.createUser

getUser :: DB.Handle -> UserId -> IO (Maybe User)
getUser h = runTransactionRO h . DUsers.selectUserById

getUsers :: DB.Handle -> PageSpec -> IO [User]
getUsers h page = toList <$> runTransactionRO h (DUsers.selectUsers page)

getUserAuthData :: DB.Handle -> UserId -> IO (Maybe UserAuthData)
getUserAuthData h = runTransactionRO h . DUsers.selectUserAuthData

deleteUser ::
     DB.Handle -> UserId -> PageSpec -> IO (Either IDeleteUser.Failure ())
deleteUser h uid defaultRange =
  runSession h $ DUsers.deleteUser uid defaultRange
