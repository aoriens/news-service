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
  , categoryListPresenter
  ) where

import Core.Authentication
import Core.Author
import Core.Category
import Core.Image
import Core.News
import Core.User
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Encoding as T
import Web.AppURI
import Web.Application
import Web.Representation.Author
import Web.Representation.Category
import Web.Representation.News
import Web.Representation.User
import Web.RepresentationBuilder
import Web.Response

authorCreatedPresenter :: AppURIConfig -> RepBuilderHandle -> Author -> Response
authorCreatedPresenter uriConfig h author =
  resourceCreatedAndReturnedResponse uriConfig (authorURI author) .
  runRepBuilder h $
  authorRep author

authorUpdatedPresenter :: AppURIConfig -> RepBuilderHandle -> Author -> Response
authorUpdatedPresenter uriConfig h author =
  resourceModifiedAndReturnedResponse uriConfig (authorURI author) .
  runRepBuilder h $
  authorRep author

authorURI :: Author -> AppURI
authorURI = AuthorURI . authorId

authorDeletedPresenter :: Response
authorDeletedPresenter = noContentResponse

authorPresenter :: RepBuilderHandle -> Author -> Response
authorPresenter h = dataResponse . runRepBuilder h . authorRep

authorListPresenter :: RepBuilderHandle -> [Author] -> Response
authorListPresenter h = dataResponse . runRepBuilder h . mapM authorRep

userCreatedPresenter ::
     AppURIConfig -> RepBuilderHandle -> User -> Credentials -> Response
userCreatedPresenter uriConfig h user creds =
  resourceCreatedAndReturnedResponse uriConfig uri . runRepBuilder h $
  userRep (Just creds) user
  where
    uri = UserURI $ userId user

userDeletedPresenter :: Response
userDeletedPresenter = noContentResponse

userPresenter :: RepBuilderHandle -> User -> Response
userPresenter h = dataResponse . runRepBuilder h . userRep Nothing

userListPresenter :: RepBuilderHandle -> [User] -> Response
userListPresenter h = dataResponse . runRepBuilder h . mapM (userRep Nothing)

imagePresenter :: Image -> Response
imagePresenter Image {..} =
  dataResponse
    ResourceRepresentation
      { resourceRepresentationBody = BB.byteString imageData
      , resourceRepresentationContentType =
          contentType $ T.encodeUtf8 imageContentType
      }

newsListPresenter :: RepBuilderHandle -> [News] -> Response
newsListPresenter h = dataResponse . runRepBuilder h . mapM newsRep

categoryCreatedPresenter ::
     AppURIConfig -> RepBuilderHandle -> Category -> Response
categoryCreatedPresenter uriConfig h category =
  resourceCreatedAndReturnedResponse uriConfig (categoryURI category) .
  runRepBuilder h $
  categoryRep category

categoryPresenter :: RepBuilderHandle -> Category -> Response
categoryPresenter h = dataResponse . runRepBuilder h . categoryRep

categoryListPresenter :: RepBuilderHandle -> [Category] -> Response
categoryListPresenter h = dataResponse . runRepBuilder h . mapM categoryRep

categoryURI :: Category -> AppURI
categoryURI cat = CategoryURI $ categoryId cat
