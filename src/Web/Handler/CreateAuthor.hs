{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateAuthor
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
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
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle m =
  Handle
    { hCreateAuthor :: AuthenticatedUser -> UserId -> T.Text -> m (Either I.Failure Author)
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> m a
    , hPresent :: Author -> Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> GenericApplication m
run Handle {..} request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  inAuthor <- hLoadJSONRequestBody request
  result <-
    hCreateAuthor authUser (UserId $ inUserId inAuthor) (inDescription inAuthor)
  author <-
    case result of
      Left I.UnknownUserId ->
        throwM $ IncorrectParameterException "Unknown UserId"
      Right a -> pure a
  respond $ hPresent author

data InAuthor =
  InAuthor
    { inUserId :: Int32
    , inDescription :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InAuthor)
