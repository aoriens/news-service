{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateUser
  ( run
  , Handle(..)
  ) where

import Core.Authentication
import qualified Core.Interactor.CreateUser as I
import Core.User
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import Data.Text (Text)
import Web.Application
import Web.Representation.Image

data Handle m =
  Handle
    { hCreateUser :: I.Request -> m (User, Credentials)
    , hPresent :: User -> Credentials -> Response
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> m a
    }

run :: Monad m => Handle m -> GenericApplication m
run Handle {..} request respond = do
  userEntity <- hLoadJSONRequestBody request
  (user, credentials) <- hCreateUser (createUserRequestFromInUser userEntity)
  respond $ hPresent user credentials

createUserRequestFromInUser :: InUser -> I.Request
createUserRequestFromInUser InUser {..} =
  I.Request
    { rFirstName = inFirstName
    , rLastName = inLastName
    , rAvatar = imageFromRep <$> inAvatar
    }

data InUser =
  InUser
    { inFirstName :: Maybe Text
    , inLastName :: Text
    , inAvatar :: Maybe ImageRep
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InUser)
