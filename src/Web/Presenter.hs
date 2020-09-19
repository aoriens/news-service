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
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Web.Representation.Author (authorRepresentation)
import Web.Representation.News (newsRepresentation)
import Web.Representation.User (userRepresentation)
import Web.RepresentationBuilder

authorCreatedPresenter :: RepBuilderHandle -> Author -> Wai.Response
authorCreatedPresenter h = runRepBuilder h . authorRepresentation

authorUpdatedPresenter :: RepBuilderHandle -> Author -> Wai.Response
authorUpdatedPresenter h = runRepBuilder h . authorRepresentation

authorDeletedPresenter :: Wai.Response
authorDeletedPresenter = noContent

authorPresenter :: RepBuilderHandle -> Author -> Wai.Response
authorPresenter h = runRepBuilder h . authorRepresentation

authorListPresenter :: RepBuilderHandle -> [Author] -> Wai.Response
authorListPresenter h = runRepBuilder h . mapM authorRepresentation

userCreatedPresenter :: RepBuilderHandle -> User -> Credentials -> Wai.Response
userCreatedPresenter h user creds =
  runRepBuilder h $ userRepresentation (Just creds) user

userDeletedPresenter :: Wai.Response
userDeletedPresenter = noContent

userPresenter :: RepBuilderHandle -> User -> Wai.Response
userPresenter h = runRepBuilder h . userRepresentation Nothing

userListPresenter :: RepBuilderHandle -> [User] -> Wai.Response
userListPresenter h = runRepBuilder h . mapM (userRepresentation Nothing)

imagePresenter :: Image -> Wai.Response
imagePresenter Image {..} =
  Wai.responseBuilder
    Http.ok200
    [(Http.hContentType, T.encodeUtf8 imageContentType)]
    (BB.byteString imageData)

newsListPresenter :: RepBuilderHandle -> [News] -> Wai.Response
newsListPresenter h = runRepBuilder h . mapM newsRepresentation

noContent :: Wai.Response
noContent = Wai.responseLBS Http.noContent204 [] mempty
