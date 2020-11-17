{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Handler.CreateComment
  ( run
  , Handle(..)
  ) where

import Core.Authentication
import Core.Comment
import qualified Core.Interactor.CreateComment as I
import Core.News
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Application
import Web.Credentials

data Handle =
  Handle
    { hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> IO a
    , hAuthenticationHandle :: AuthenticationHandle IO
    , hCreateCommentHandle :: I.Handle IO
    , hPresenter :: Comment -> Response
    }

run :: Handle -> NewsId -> Application
run Handle {..} newsId' request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  InComment {inText} <- hLoadJSONRequestBody request
  resultingComment <- I.run hCreateCommentHandle authUser inText newsId'
  respond $ hPresenter resultingComment

newtype InComment =
  InComment
    { inText :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InComment)
