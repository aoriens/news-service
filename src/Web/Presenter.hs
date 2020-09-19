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
  ) where

import Core.Authentication
import Core.Author
import Core.Image
import Core.News
import Core.User
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Encoding as T
import qualified Network.Wai as Wai
import Web.AppURI
import Web.Representation.Author (authorRepresentation)
import Web.Representation.News (newsRepresentation)
import Web.Representation.User (userRepresentation)
import Web.RepresentationBuilder
import Web.Response

authorCreatedPresenter ::
     AppURIConfig -> RepBuilderHandle -> Author -> Wai.Response
authorCreatedPresenter uriConfig h author =
  resourceCreatedAndReturnedResponse uriConfig (authorURI author) .
  runRepBuilder h $
  authorRepresentation author

authorUpdatedPresenter ::
     AppURIConfig -> RepBuilderHandle -> Author -> Wai.Response
authorUpdatedPresenter uriConfig h author =
  resourceModifiedAndReturnedResponse uriConfig (authorURI author) .
  runRepBuilder h $
  authorRepresentation author

authorURI :: Author -> AppURI
authorURI = AuthorURI . authorId

authorDeletedPresenter :: Wai.Response
authorDeletedPresenter = noContentResponse

authorPresenter :: RepBuilderHandle -> Author -> Wai.Response
authorPresenter h = dataResponse . runRepBuilder h . authorRepresentation

authorListPresenter :: RepBuilderHandle -> [Author] -> Wai.Response
authorListPresenter h =
  dataResponse . runRepBuilder h . mapM authorRepresentation

userCreatedPresenter ::
     AppURIConfig -> RepBuilderHandle -> User -> Credentials -> Wai.Response
userCreatedPresenter uriConfig h user creds =
  resourceCreatedAndReturnedResponse uriConfig uri . runRepBuilder h $
  userRepresentation (Just creds) user
  where
    uri = UserURI $ userId user

userDeletedPresenter :: Wai.Response
userDeletedPresenter = noContentResponse

userPresenter :: RepBuilderHandle -> User -> Wai.Response
userPresenter h = dataResponse . runRepBuilder h . userRepresentation Nothing

userListPresenter :: RepBuilderHandle -> [User] -> Wai.Response
userListPresenter h =
  dataResponse . runRepBuilder h . mapM (userRepresentation Nothing)

imagePresenter :: Image -> Wai.Response
imagePresenter Image {..} =
  dataResponse
    ResourceRepresentation
      { resourceRepresentationBody = BB.byteString imageData
      , resourceRepresentationContentType =
          contentType $ T.encodeUtf8 imageContentType
      }

newsListPresenter :: RepBuilderHandle -> [News] -> Wai.Response
newsListPresenter h = dataResponse . runRepBuilder h . mapM newsRepresentation
