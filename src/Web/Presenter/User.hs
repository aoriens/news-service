module Web.Presenter.User
  ( presentUser
  , presentUsers
  , RepBuilderHandle(..)
  ) where

import qualified Core.Authentication as Auth
import qualified Core.User as C
import qualified Data.ByteString.Builder as BB
import Web.Representation.User
import Web.RepresentationBuilder

presentUser ::
     RepBuilderHandle -> C.User -> Maybe Auth.Credentials -> BB.Builder
presentUser h user creds = runRepBuilder h $ userRepresentation creds user

presentUsers :: RepBuilderHandle -> [C.User] -> BB.Builder
presentUsers h = runRepBuilder h . mapM (userRepresentation Nothing)
