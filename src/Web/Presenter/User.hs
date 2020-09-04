module Web.Presenter.User
  ( presentUser
  , presentUsers
  , RB.Handle(..)
  ) where

import qualified Core.Authentication as Auth
import qualified Core.User as C
import qualified Data.ByteString.Builder as BB
import Web.Representation.User
import qualified Web.RepresentationBuilder as RB

presentUser :: RB.Handle -> C.User -> Maybe Auth.Credentials -> BB.Builder
presentUser h user creds = RB.runBuilder h $ userRepresentation creds user

presentUsers :: RB.Handle -> [C.User] -> BB.Builder
presentUsers h = RB.runBuilder h . mapM (userRepresentation Nothing)
