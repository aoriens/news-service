{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateAuthor
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Authentication
import Core.Author
import qualified Core.Interactor.CreateAuthor as I
import Core.User
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Application
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hCreateAuthorHandle :: I.Handle IO
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> IO a
    , hPresent :: Author -> Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> Application
run Handle {..} request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  inAuthor <- hLoadJSONRequestBody request
  result <-
    I.run
      hCreateAuthorHandle
      authUser
      (UserId $ iaUserId inAuthor)
      (iaDescription inAuthor)
  author <-
    case result of
      Left I.UnknownUserId ->
        throwIO $ IncorrectParameterException "Unknown UserId"
      Right a -> pure a
  respond $ hPresent author

data InAuthor =
  InAuthor
    { iaUserId :: Int32
    , iaDescription :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "ia"}
    ''InAuthor)
