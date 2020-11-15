{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.PatchAuthor
  ( run
  , Handle(..)
  ) where

import Core.Authentication
import Core.Author
import qualified Core.Interactor.UpdateAuthor as I
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Application
import Web.Credentials

data Handle =
  Handle
    { hUpdateAuthorHandle :: I.Handle IO
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> IO a
    , hPresenter :: Author -> Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> AuthorId -> Application
run Handle {..} authorId' request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  InAuthor {inDescription} <- hLoadJSONRequestBody request
  newAuthor <- I.run hUpdateAuthorHandle authUser authorId' inDescription
  respond $ hPresenter newAuthor

newtype InAuthor =
  InAuthor
    { inDescription :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InAuthor)
