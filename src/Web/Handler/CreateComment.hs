{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Handler.CreateComment
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Comment
import Core.News
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Application
import Web.Credentials hiding (Credentials)

data Handle m =
  Handle
    { hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> m a
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    , hCreateComment :: AuthenticatedUser -> T.Text -> NewsId -> m Comment
    , hPresent :: Comment -> Response
    }

run :: MonadThrow m => Handle m -> NewsId -> GenericApplication m
run Handle {..} newsId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  InComment {inText} <- hLoadJSONRequestBody request
  resultingComment <- hCreateComment authUser inText newsId
  respond $ hPresent resultingComment

newtype InComment =
  InComment
    { inText :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InComment)
