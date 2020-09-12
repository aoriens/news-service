module Web.Presenter.User
  ( presentUser
  , presentUsers
  , RepBuilderHandle(..)
  ) where

import qualified Core.Authentication as Auth
import qualified Core.User as C
import Network.Wai as Wai
import Web.Representation.User
import Web.RepresentationBuilder

presentUser ::
     RepBuilderHandle -> C.User -> Maybe Auth.Credentials -> Wai.Response
presentUser h user creds = runRepBuilder h $ userRepresentation creds user

presentUsers :: RepBuilderHandle -> [C.User] -> Wai.Response
presentUsers h = runRepBuilder h . mapM (userRepresentation Nothing)
