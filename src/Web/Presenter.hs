module Web.Presenter
  ( authorCreatedPresenter
  , authorUpdatedPresenter
  , authorDeletedPresenter
  , authorPresenter
  , authorListPresenter
  , userCreatedPresenter
  , userDeletedPresenter
  , userPresenter
  , userListPresenter
  , imagePresenter
  , newsListPresenter
  , categoryCreatedPresenter
  , categoryPresenter
  ) where

import Core.Authentication
import Core.Author
import Core.Category
import Core.Image
import Core.News
import Core.User
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import Web.AppURI
import Web.Representation.Author
import Web.Representation.Category
import Web.Representation.News
import Web.Representation.User
import Web.RepresentationBuilder
import Web.Response

authorCreatedPresenter ::
     AppURIConfig -> RepBuilderHandle -> Author -> Wai.Response
authorCreatedPresenter uriConfig h author =
  resourceCreatedAndReturnedResponse uriConfig (authorURI author) .
  runRepBuilder h $
  authorRep author

authorUpdatedPresenter ::
     AppURIConfig -> RepBuilderHandle -> Author -> Wai.Response
authorUpdatedPresenter uriConfig h author =
  resourceModifiedAndReturnedResponse uriConfig (authorURI author) .
  runRepBuilder h $
  authorRep author

authorURI :: Author -> AppURI
authorURI = AuthorURI . authorId

authorDeletedPresenter :: Wai.Response
authorDeletedPresenter = noContentResponse

authorPresenter :: RepBuilderHandle -> Author -> Wai.Response
authorPresenter h = dataResponse . runRepBuilder h . authorRep

authorListPresenter :: RepBuilderHandle -> [Author] -> Wai.Response
authorListPresenter h = dataResponse . runRepBuilder h . mapM authorRep

userCreatedPresenter ::
     AppURIConfig -> RepBuilderHandle -> User -> Credentials -> Wai.Response
userCreatedPresenter uriConfig h user creds =
  resourceCreatedAndReturnedResponse uriConfig uri . runRepBuilder h $
  userRep (Just creds) user
  where
    uri = UserURI $ userId user

userDeletedPresenter :: Wai.Response
userDeletedPresenter = noContentResponse

userPresenter :: RepBuilderHandle -> User -> Wai.Response
userPresenter h = dataResponse . runRepBuilder h . userRep Nothing

userListPresenter :: RepBuilderHandle -> [User] -> Wai.Response
userListPresenter h = dataResponse . runRepBuilder h . mapM (userRep Nothing)

imagePresenter :: Image -> Wai.Response
imagePresenter Image {..} =
  dataResponse
    ResourceRepresentation
      { resourceRepresentationBody = BB.byteString imageData
      , resourceRepresentationContentType =
          contentType $ T.encodeUtf8 imageContentType
      }

newsListPresenter :: RepBuilderHandle -> [News] -> Wai.Response
newsListPresenter h = dataResponse . runRepBuilder h . mapM newsRep

categoryCreatedPresenter ::
     AppURIConfig -> RepBuilderHandle -> Category -> Wai.Response
categoryCreatedPresenter uriConfig h category =
  resourceCreatedAndReturnedResponse uriConfig (categoryURI category) .
  runRepBuilder h $
  categoryRep category

categoryPresenter :: RepBuilderHandle -> Category -> Wai.Response
categoryPresenter h = dataResponse . runRepBuilder h . categoryRep

categoryURI :: Category -> AppURI
categoryURI cat = CategoryURI $ categoryId cat
